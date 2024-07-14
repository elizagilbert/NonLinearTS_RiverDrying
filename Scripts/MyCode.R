library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(animation)

#All Data _ Isleta ####
ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date) %>%   
  filter(between(month(dates), 4, 10))
x.obs <- ts$ExtentDry
x<-standardize(x.obs)   #standardize data
dates<-ts$dates

#Fourier Power Spectrum ####
dump("spectral_udf", file="Functions/spectral_udf.R")
source("Functions/spectral_udf.R")

results2<-spectral(x,method_spec="ar")

# Table of Fourier Results
cycle_lengths1<-na.omit(unlist(results2)[1:4])
frequencies1<-1/cycle_lengths1
fourier_table2<-data.frame(rbind(cycle_lengths1,frequencies1))
fourier_table3<-t(fourier_table2)
colnames(fourier_table3)<-c("cycle_lengths","frequencies")
head(fourier_table3)


#Singular Spectrum Analysis ####
setwd(dir.udf)
dump("SSA_udf", file="SSA_udf.R");source("SSA_udf.R")  #SSA

output<-SSA(x)  #run SSA_udf
w.corr2<-output[[1]]
groups2<-output[[2]]
reconstruction2<-output[[3]]
cycles2<-output[[4]]
cycle.lengths2<-cycles2[1,][-ncol(cycles2)]
colnames(reconstruction2)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

IsletaSignal <- reconstruction2$signal

#save the signal
setwd(dir.results)
write.csv(IsletaSignal, "IsletaSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function ####
mutual.out <- mutual(IsletaSignal) #mutual(tseriesChaos) Embedding delay = d 

setwd(dir.udf)
dump("embed_delay_udf", file="embed_delay_udf.R");source("embed_delay_udf.R")
d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(IsletaSignal,m=3,d=d,idt=1,mdt=length(IsletaSignal))


## Isolate observations of highest contour
contour_10<-out[10,1:1000]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- 500

#from tseriesChaos
m.max <- 6 #max number of embedding dimensions to consider
fn.out <- false.nearest(IsletaSignal, m.max, d, tw)
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding ~4

#Time-delay embedding
m <- 4
Mx <- embedd(IsletaSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,2)) 
embedding(IsletaSignal)
scatterplot3d(Mx, type = "l", main="shadow isleta irrigation months")

#surrogate data testing
if(!require(robustHD)) { install.packages('robustHD') }
library(robustHD)  #embedding
if(!require(tseriesChaos)) { install.packages('tseriesChaos') }
library(tseriesChaos)  #embedding
if(!require(nonlinearTseries)) { install.packages('nonlinearTseries') } 
library(nonlinearTseries)
if(!require(pdc)) { install.packages('pdc') }
library(pdc)  #permutation entropy
if(!require(fields)) { install.packages('fields') }; library(fields)
if(!require(R.devices)) { install.packages('R.devices') }; library(R.devices)

###############################
# Load user defined functions #
###############################
setwd(dir.udf)
dump("embed_udf", file="embed_udf.R");source("embed_udf.R")
dump("predict_np_udf", file="predict_np_udf.R");source("predict_np_udf.R")

#####################################
# Import Observed Times Series Data #
#####################################
setwd(dir.results)
x <- read.csv("IsletaSignal.csv")
x <- as.matrix(x)


########
# Code #
########

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
} #end if np

# Compute Permutation Entropy
if(pe) {
  entropy<-entropyHeuristic(x,m.min=3,m.max=7,t.min=1,t.max=20)
  entropy.values<-unlist(entropy$entropy.values[,3])
  entropy.values<-na.omit(entropy.values)
  entropy.x<-min(entropy.values)
  print("entropy.x"); print(entropy.x)
} #end if pe

## Generate surrogates
surrogates<-matrix(0,length(x),n.surrogates) #surrogates for 1-sided test

for(i in 1:n.surrogates){ 
  surr_out<-FFTsurrogate(x,1)
  surr<-surr_out[1:length(surr_out)]
  surrogates[,i]<-surr 
}  #end loop i

surr <- surrogates[1:6, 1:5]
print("Surrogates (subset)"); print(surr)

################################################
# Chunk 3: Surrogate Discriminating Statistics #
################################################

## Upper-tailed test for nonlinear prediction skill (nse)
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
} #end if np

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
} #end if pe

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
} #end np, pe

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

setwd(dir.results)
write.table(surr.table,"IsletaIrrigAllYears_Hypothesis.table.csv",col.names=NA,sep=",")

print("Hypothesis Table");print(surr.table)

