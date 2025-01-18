# Libraries #####
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
library(beepr)

# Load user defined functions #####
dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
dump("predict_np_udf", file="Functions/predict_np_udf.R"); source("Functions/predict_np_udf.R")
dump("embed_udf", file="Functions/embed_udf.R"); source("Functions/embed_udf.R")


#Code ####
x <- as.matrix(read.csv("Results/Signal/SanAcacia_DischSignal.csv"))

## embeding values  
d<-d_udf(x)  #compute average mutual information function with udf embed_delay_udf

## Space-time separation plot
par(mfrow=c(2,1))  #arrange upcoming plots in two rows
out<-stplot(x,m=3,d=d,idt=1,mdt=length(x))

## Isolate first 100 observations of highest contour
contour_10<-out[10,1:100]
plot(contour_10,type='l')

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(x,m=3,d=d,idt=1,mdt=length(x))


## Isolate observations of highest contour
contour_10<-out[10,1:365]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))

## Embedded data matrix
results<-embed_udf(x,tw)
d<-results[[1]]  #embedding delay
m<-results[[2]]  #embedding dimension
Mx<-results[[3]] #embedded data matrix

Mx_towrite <- Mx[,1:3]

write.csv(Mx_towrite, "Results/Mx/TWSanAcacia_discharge_Mx.csv", row.names = F)

## Hypothesis testing parameters
np<-T    #T: run nonlinear prediction skill as discriminating statistic
pe<-T    #T: compute permutation entropy as discrimating statistic
k=5     #Set number of surrogates for one side tests
alpha=0.05  #significance level
n.surrogates=(k/alpha)-1  #Number of surrogates for one-sided hypothesis test
lb=0.01;ub=0.4 #upper and lower bounds to use if PPS selected
frac.learn<-0.4  #percent of rows used in learning set for nonlinear prediction

## Signal: Discriminating statistics

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


write.table(surr.table,"Results/Surrogate/Huffaker/SurrogateTest_SanAcaiaDischarge_Huff.csv",col.names=NA,sep=",")

print("Hypothesis Table");print(surr.table)