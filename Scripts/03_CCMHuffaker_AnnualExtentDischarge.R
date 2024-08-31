#Read me####
#using huffaker code to run CCM. Tested using a random sequence of data and p-value
#was 0.81 but extended CCM did not show any peaks in rho

#Libraries ####
library(rEDM)
library(tidyverse)
library(lubridate)
library(beepr)
library(tseriesChaos)#embedding
if(!require(multispatialCCM)) { install.packages('multispatialCCM') }
library(multispatialCCM)  #CCM package says can use multiple short time series and bootstrapping stitches it together
if(!require(R.devices)) { install.packages('R.devices') }
library(R.devices)  #ignore graphics


#data ####

#processed signal from Huffaker
#for rEDM have to have a date column and it needs to be before the variable
IsletaSign <- read.csv("Results/IsletaSignal_Irrig.csv") %>% 
  rename(Extent = x) %>% 
  mutate(Extent = as.numeric(Extent)) 


IsletaDischSign <- read.csv("Results/Signal_Discharge/IsletaDischargeSignal_Irrig.csv") %>% 
  rename(Discharge = 1)

##CCM: Does A cause B? (B xmap A) ####
lib_sequence<-5  #by argument in sequence statement
A<-IsletaDischSign$Discharge #forcing process  
B<-IsletaSign$Extent     #response process: calculate d, m, and tw from this time series


# theiler window for B ####
mutual.out <- mutual(IsletaSign$Extent , lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
                  rownames_to_column() %>% 
                  filter(x == min(x)) %>% 
                  rename(Emdelay = 1) %>% 
                  select(Emdelay))

par(mfrow=c(1,2))  
out<-stplot(IsletaSign$Exten,m=3,d=d,idt=1,mdt=length(IsletaDischSign$Discharge))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))

## Extended CCM ####
num.bd<-10   #Delayed CCM: max backward time delay
num.fd<-10   #Delayed CCM: max forward time dealy


###############################
# Load user defined functions #
###############################
dump("embed_udf", file="Functions/embed_udf.R"); source("Functions/embed_udf.R")

########
# Code #
########
## Embed response variable B
nulldev()
results.embed<-embed_udf(B,tw)
dev.off()
d<-results.embed[[1]]
m<-results.embed[[2]]

## Run CCM: Does A cause B?
lib<-seq((d*(m-1)+(m+1)),(length(A)-m+2),lib_sequence)
ccm.xy<-CCM_boot(A,B,m,d,DesiredL=lib,iterations=100) #takes about 20 minutes
ccm_rho<-ccm.xy$rho

## Statistical signficance
# ccm.xy is significant at the alpha=0.01 level if pvalue <= 0.01
sig.test<-ccmtest(ccm.xy,ccm.xy)
pvalue<-sig.test[1]

################
# EXTENDED-CCM #
################
#Run test on CCMs passing convergence test for each delay 'tp'
#Rules out 'generalized synchrony' masquerading as causal interaction

###################
#CCM without delay#
###################
ccm.0<-CCM_boot(A,B,m,d,DesiredL=lib[length(lib)],iterations=100) #takes a few seconds
rho.0<-ccm.0$rho  #ccm correlation coefficient 

##########################
#CCM with backward delays#
##########################
rho_b_vec<-matrix(0,num.bd,1)
for(i1 in 1:num.bd){
  #print("i1");print(i1)
  embed<-embedd(B,2,i1)  #put backward delay into response variable
  B.b<-embed[,2]  #response variable
  A.b<-A[1:length(B.b)]  #driving variable
  ccm.b<-CCM_boot(A.b,B.b,m,d,DesiredL=lib[length(lib)],iterations=100)
  rho.b<-ccm.b$rho  #ccm correlation coefficients for each library
  #print("rho.b");print(rho.b)
  rho_b_vec[i1,]<-rho.b
}  #end loop i1 #takes less than a minute

#########################
#CCM with forward delays#
#########################
rho_f_vec<-matrix(0,num.fd,1)
for(i2 in 1:num.fd){
  #print("i2");print(i2)
  embed<-embedd(A,2,i2) #put forward delay into driving variable
  A.f<-embed[,2]
  B.f<-B[1:length(A.f)]
  ccm.f<-CCM_boot(A.f,B.f,m,d,DesiredL=lib[length(lib)],iterations=100)
  rho.f<-ccm.f$rho  #ccm correlation coefficients for each library
  #print("rho.f");print(rho.f)
  rho_f_vec[i2,]<-rho.f
  #print("rho_f_vec");print(rho_f_vec)
}  #end loop i2

#'rho.delay.matrix' reports rho's for each cross mapping (columns) and for each delay (rows)
results.delay.1<-rbind(rho_b_vec,rho.0,rho_f_vec)
col.1<--num.bd:num.fd
rho.delay.matrix<-as.data.frame(cbind(col.1,results.delay.1),ncol=2,row.names=FALSE)
colnames(rho.delay.matrix)<-c("delays","ccm");rho.delay.matrix
rho.delay.matrix.rounded<-round(rho.delay.matrix[,2],1)
o<-order(rho.delay.matrix.rounded,decreasing=TRUE)
delay.max<-rho.delay.matrix[,1][o][1]

#########
#Results#
#########
# CCM plot
lib_plot<-lib[1:length(ccm_rho)]
plot(lib_plot,ccm_rho,main="B xmap A",xlab="library",ylab="rho")

# Statistical significance: pvalue <= alpha (e.g., 0.01)
pvalue

# plot results of Extended-CCM
plot(rho.delay.matrix[,1],rho.delay.matrix.rounded,type='l',
     main="Extended-CCM",xlab="delays",ylab="rho")

# Pass Extended-CCM: delay.max <= 0
delay.max
