library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(zoo)
library(dataRetrieval)
if(!require(nonlinearTseries)) { install.packages('nonlinearTseries') } 
library(nonlinearTseries)
if(!require(pdc)) { install.packages('pdc') }
library(pdc)  #permutation entropy
if(!require(fields)) { install.packages('fields') }; library(fields)
if(!require(R.devices)) { install.packages('R.devices') }; library(R.devices)
library(beepr)

# Load user defined functions #####
dump("embed_udf", file="Functions/embed_udf.R"); source("Functions/embed_udf.R")
dump("predict_np_udf", file="Functions/predict_np_udf.R"); source("Functions/predict_np_udf.R")
#dump("PPS_generate_udf", file="Functions/PPS_generate_udf.R"); source("Functions/PPS_generate_udf.R")

#data #####
# Define ARIMA model parameters and sinusoidal cycle parameters
n <- 2*365 # Four years of daily data
phi <- 0.5  # Coefficient for the AR(1) term
theta <- 0.5  # Coefficient for the MA(1) term
set.seed(123)  # For reproducibility

# ARIMA base series
arima_series <- arima.sim(n = n, model = list(ar = phi, ma = theta, order = c(1, 0, 1)))

# Parameters for cycles
frequency1 <- 2 * pi / 50  # Cycle of about 50 days
frequency2 <- 2 * pi / 365  # Cycle of about one year
amplitude1 <- 0.5 * sd(arima_series)  # Making amplitude significant
amplitude2 <- 0.3 * sd(arima_series)  # Making amplitude significant

# Adding cycles to the ARIMA series
arima_series_with_cycles <- arima_series +
  amplitude1 * sin(frequency1 * 1:n) +
  amplitude2 * sin(frequency2 * 1:n)

# Convert to a time series object
ts_arima_with_cycles <- ts(arima_series_with_cycles, frequency = 365)

# Assuming the time series starts on January 1, 2020
start_date <- as.Date("2020-01-01")

# Assuming daily frequency
dates <- seq(from = start_date, length.out = length(ts_arima_with_cycles), by = "day")


ts <- ts_arima_with_cycles #change based on what gage using
x.obs <- ts_arima_with_cycles
x<-standardize(x.obs)   #standardize data
dates<-dates



#Singular Spectrum Analysis ####
dump("SSA_udf", file="Functions/SSA_udf.R");source("Functions/SSA_udf.R")  #SSA

output<-SSA(x)  #run SSA_udf
w.corr2<-output[[1]]
groups2<-output[[2]]
reconstruction2<-output[[3]]
cycles2<-output[[4]]
cycle.lengths2<-cycles2[1,][-ncol(cycles2)]
colnames(reconstruction2)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

DischargeSignal <- reconstruction2$signal

#Embedding delay with Mutual Information Function ####
mutual.out <- mutual(DischargeSignal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
  rownames_to_column() %>% 
  filter(x == min(x)) %>% 
  rename(Emdelay = 1) %>% 
  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(DischargeSignal,m=3,d=d,idt=1,mdt=length(DischargeSignal))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 5 #max number of embedding dimensions to consider
fn.out <- false.nearest(DischargeSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:5])) 
  

#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(DischargeSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(DischargeSignal)
scatterplot3d(Mx, type = "l", main="shadow discharge")

## Hypothesis testing parameters
np<-T    #T: run nonlinear prediction skill as discriminating statistic
pe<-T    #T: compute permutation entropy as discrimating statistic
k=5     #Set number of surrogates for one side tests
alpha=0.05  #significance level
n.surrogates=(k/alpha)-1  #Number of surrogates for one-sided hypothesis test
lb=0.01;ub=0.4 #upper and lower bounds to use if PPS selected
frac.learn<-0.4  #percent of rows used in learning set for nonlinear prediction

# Import Observed Times Series Data #
x <- x 
x <- as.matrix(x)

## Signal: Discriminating statistics

# Embed signal
results.embed<-embed_udf(x,tw=tw)
Mx<-results.embed[[3]] #embedded data matrix
print("Mx"); head(Mx)

# Compute Nonlinear Prediction Skill  
if(np) {
  results.np<-predict_np_udf(Mx,frac.learn=frac.learn)
  nse.x<-results.np[[1]]
  print("nse.x"); print(nse.x)
} #end if np - takes a few minutes to run

# Compute Permutation Entropy
if(pe) {
  entropy<-entropyHeuristic(x,m.min=3,m.max=7,t.min=1,t.max=20)
  entropy.values<-unlist(entropy$entropy.values[,3])
  entropy.values<-na.omit(entropy.values)
  entropy.x<-min(entropy.values)
  print("entropy.x"); print(entropy.x)
} #end if pe - takes seconds to run

## Generate surrogates
surrogates<-matrix(0,length(x),n.surrogates) #surrogates for 1-sided test

for(i in 1:n.surrogates){ 
  surr_out<-FFTsurrogate(x,1)
  surr<-surr_out[1:length(surr_out)]
  surrogates[,i]<-surr 
}  #end loop i

surr <- surrogates[1:6, 1:5]
print("Surrogates (subset)"); print(surr)

# Chunk 3: Surrogate Discriminating Statistics #

## Upper-tailed test for nonlinear prediction skill (nse)
sys.start <- Sys.time()
if(np) {
  nse.surr<-matrix(0,n.surrogates,1)  #Nash-Sutcliffe Efficiency for each surrogate
  for(i1 in 1:n.surrogates) {
    Mw2<-embedd(surrogates[,i1],m,d) #embed data matrix with same parameters as signal
    results.np<-predict_np_udf(Mw2,frac.learn=frac.learn)
    nse.surr[i1,]<-results.np[[1]] #nse
  }  #end loop i1
  nse.surr<-na.omit(nse.surr)
  
  # Rank-order test
  nse.low<-NA
  nse.high<-sort(nse.surr)[length(nse.surr)-(k-1)]  #lower limit on k largest values
} #end if np - 5.5 hrs
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

## Lower-tailed test for entropy measure
if(pe) {
  entropy.surr<-matrix(0,n.surrogates,1)  #Nash-Sutcliffe Efficiency for each surrogate
  for(i2 in 1:n.surrogates) {
    entropy<-entropyHeuristic(surrogates[,i2],m.min=3,m.max=7,t.min=1,t.max=20)
    entropy.val<-unlist(entropy$entropy.values[,3])
    entropy.val<-na.omit(entropy.val)
    entropy.min<-min(entropy.val)
    entropy.surr[i2,]<-entropy.min #entropy value
  }  #end loop i2
  entropy.surr<-na.omit(entropy.surr)
  
  # Rank order test
  entropy.low<-sort(entropy.surr)[k]  #upper limit on k lowest values
  entropy.high<-NA
} #end if pe - few seconds


## Prepare table of surrogate results 

if(np&&pe) {  #np, pe
  #Hypothesis results
  nse.H0<-if(nse.x<nse.high) {"Stochastic"} else {"Deterministic"}
  entropy.H0<-if(entropy.x>entropy.low) {"Stochastic"} else {"Deterministic"}
  H0<-c(nse.H0,entropy.H0)
  #Round discriminating statistics
  nse.x<-round(nse.x,digits=3);nse.high<-round(nse.high,digits=3)
  entropy.x<-round(entropy.x,digits=3);entropy.low<-round(entropy.low,digits=3)
  #Format rows of table
  row.nse<-c(nse.x,NA,nse.high,nse.H0)
  row.entropy<-c(entropy.x,entropy.low,NA,entropy.H0)
  row.k<-c(k,NA,NA,NA)
  row.alpha<-c(alpha,NA,NA,NA)
  #Format surrogate table
  surr.table<-rbind(row.nse,row.entropy,row.k,row.alpha)
  row.names(surr.table)<-c("Predictive skill","Entropy","k","alpha")
  colnames(surr.table)<-c("Time Series","surr(low)","surr(high)","H0")
} #end np, pe - few seconds


if(np&&!pe) {  #np
  #Hypothesis results
  nse.H0<-if(nse.x<nse.high) {"Stochastic"} else {"Deterministic"}
  H0<-c(nse.H0)
  #Round discriminating statistics
  nse.x<-round(nse.x,digits=3);nse.high<-round(nse.high,digits=3)
  #Format rows of table
  row.nse<-c(nse.x,NA,nse.high,nse.H0)
  row.k<-c(k,NA,NA,NA)
  row.alpha<-c(alpha,NA,NA,NA)
  #Format surrogate table
  surr.table<-rbind(row.nse,row.k,row.alpha)
  row.names(surr.table)<-c("Predictive skill","k","alpha")
  colnames(surr.table)<-c("Time Series","surr(low)","surr(high)","H0")
} #end np


if(!np&&pe) {  #pe
  #Hypothesis results
  entropy.H0<-if(entropy.x>entropy.low) {"Stochastic"} else {"Deterministic"}
  H0<-c(entropy.H0)
  #Round discriminating statistics
  entropy.x<-round(entropy.x,digits=3);entropy.low<-round(entropy.low,digits=3)
  #Format rows of table
  row.entropy<-c(entropy.x,entropy.low,NA,entropy.H0)
  row.k<-c(k,NA,NA,NA)
  row.alpha<-c(alpha,NA,NA,NA)
  #Format surrogate table
  surr.table<-rbind(row.entropy,row.k,row.alpha)
  row.names(surr.table)<-c("Entropy","k","alpha")
  colnames(surr.table)<-c("Time Series","surr(low)","surr(high)","H0")
} #end pe

print("Hypothesis Table");print(surr.table)

#lyapunov ####
length(lorenz.ts)

output <-lyap_k(lorenz.ts, m=3, d=2, s=200, t=40, ref=1700, k=2, eps=4)
plot(output)
lyap(output, 1, 1.5)


test <- lyap_k(arima_series_with_cycles, m=m, d=d, s=10, t=tw, ref = 50, k=2, eps=4)
plot(test)
lyap(output, 1, 1.3)


