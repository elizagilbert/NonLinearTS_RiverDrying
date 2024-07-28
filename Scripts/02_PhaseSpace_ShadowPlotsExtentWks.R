#Read me ####
#The purpose of this script is to decompose the daily extent dry to a week (time) signal
#and see what phase space and shadow plots look like
#and to see if the signal has nonlinear deterministic features
#using Huffaker code in addition to decomposition with fpp3

#Libraries ####
library(tidyverse)
library(fpp3) #times series decomposition
library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(lubridate) #data wrangling
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(Rssa) #singluar spectrum analysis
library(robustHD)  #embedding
library(pdc)  #permutation entropy
library(fields)
library(R.devices)
library(beepr)

#Functions ####
dump("SSA_udf", file="Functions/SSA_udf.R");source("Functions/SSA_udf.R")  #Singlar spectrum analysis
dump("embed_udf", file="Functions/embed_udf.R"); source("Functions/embed_udf.R")
dump("predict_np_udf", file="Functions/predict_np_udf.R"); source("Functions/predict_np_udf.R")
dump("PPS_generate_udf", file="Functions/PPS_generate_udf.R"); source("Functions/PPS_generate_udf.R")

#Data ####
extent_isl_ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date) %>%   
  select(dates, ExtentDry) %>% 
  as_tsibble(index = dates)

#STL decomposition with trend and season ####
extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(),robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()

decomp_is <- extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(), robust = TRUE)) %>% 
  components() %>% 
  select(season_week) %>% 
  filter(year(dates) <= 2015)

#signal data####
x<-decomp_is %>% 
  as.data.frame() %>% 
  select(season_week) %>% 
  standardize() %>%   #standardize data
  mutate(season_week = season_week + 12.702003)

x <- x$season_week

dates<-decomp_is$dates


#Singular Spectrum Analysis - Rssa ####
#Huffaker function - has errors
ssa_obj <- ssa(x)

# Summary of SSA results
summary(ssa_obj)

# Plot the singular values to help decide on the number of components
plot(ssa_obj, type = "values")

# Reconstruct the components (example: first 2 components)
reconstructed <- reconstruct(ssa_obj, groups = list(1:2))

# Plot the reconstructed series
plot(reconstructed, plot.method = "xyplot")

# Extract the reconstructed series if needed
IsletaSignal <- reconstructed$F1

#save the signal
#write.csv(IsletaSignal, "Results/IsletaSignal_Wk.csv", row.names = F)

#Embedding delay with Average Mutual Information (AMI) Function ####
mutual.out <- mutual(IsletaSignal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
                  rownames_to_column() %>% 
                  filter(x == min(x)) %>% 
                  rename(Emdelay = 1) %>% 
                  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(IsletaSignal,m=3,d=d,idt=1,mdt=length(IsletaSignal))

## Isolate observations of highest contour
contour_10<-out[10,1:1000]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))

#from tseriesChaos
m.max <- 10 #max number of embedding dimensions to consider
fn.out <- false.nearest(IsletaSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 


#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value


#Time-delay embedding
m <- for_m
Mx <- embedd(IsletaSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

# shadow and phase-space  ####

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,2)) 
embedding(IsletaSignal)
scatterplot3d(Mx, type = "l", main="shadow isleta")

#Surrogate testing ####
## Theiler window estimated from space-time separation plot
tw<-tw

## Hypothesis testing parameters
np<-T    #T: run nonlinear prediction skill as discriminating statistic
pe<-T    #T: compute permutation entropy as discrimating statistic
k=5     #Set number of surrogates for one side tests
alpha=0.05  #significance level
n.surrogates=(k/alpha)-1  #Number of surrogates for one-sided hypothesis test
lb=0.01;ub=0.4 #upper and lower bounds to use if PPS selected
frac.learn<-0.4  #percent of rows used in learning set for nonlinear prediction

#  Observed Times Series Data #
x <- read.csv("Results/IsletaSignal_Wk.csv")
x <- as.matrix(x)

## Signal: Discriminating statistics

# Embed signal
par(mfrow=c(1,2))  #Put two embedding figures in one row
results.embed<-embed_udf(x,tw=tw)
d<-results.embed[[1]]
m<-results.embed[[2]]
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


# Surrogate Discriminating Statistics ####
## Generate surrogates
surrogates<-matrix(0,length(x),n.surrogates) #surrogates for 1-sided test

for(i in 1:n.surrogates){ 
  surr_out<-FFTsurrogate(x,1)
  surr<-surr_out[1:length(surr_out)]
  surrogates[,i]<-surr 
}  #end loop i

surr <- surrogates[1:6, 1:5]
print("Surrogates (subset)"); print(surr)

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
} #end if np - 5 hrs Isleta all; 
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

## Lower-tailed test for entropy measure
sys.start <- Sys.time()
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
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

## Prepare table of surrogate results 
sys.start <- Sys.time()
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
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

sys.start <- Sys.time()
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
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

sys.start <- Sys.time()
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
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
beep(3)

write.table(surr.table,"Results/SurrogateTest_Isl_ExtDry_Wk.csv",col.names=NA,sep=",")

print("Hypothesis Table");print(surr.table)