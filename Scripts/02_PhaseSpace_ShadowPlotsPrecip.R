library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(zoo)
library(dataRetrieval)


#All Data _ Isleta ####
IsletaPrecip <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  #filter(between(month(dates), 4, 10)) %>% 
  select(dates, Precip_mm) %>% 
  mutate(time = 1:length(dates)) 

ts <- IsletaPrecip$Precip_mm #change based on what gage using
x.obs <- ts
x<-standardize(x.obs)   #standardize data
dates<-IsletaPrecip$dates

#Fourier Power Spectrum 
# dump("spectral_udf", file="Functions/spectral_udf.R");source("Functions/spectral_udf.R")
# 
# results2<-spectral(x,method_spec="ar")
# 
# # Table of Fourier Results
# cycle_lengths1<-na.omit(unlist(results2)[1:4])
# frequencies1<-1/cycle_lengths1
# fourier_table2<-data.frame(rbind(cycle_lengths1,frequencies1))
# fourier_table3<-t(fourier_table2)
# colnames(fourier_table3)<-c("cycle_lengths","frequencies")
# head(fourier_table3)


#Singular Spectrum Analysis 
dump("SSA_udf", file="Functions/SSA_udf.R");source("Functions/SSA_udf.R")  #SSA

output<-SSA(x)  #run SSA_udf
w.corr2<-output[[1]]
groups2<-output[[2]]
reconstruction2<-output[[3]]
cycles2<-output[[4]]
cycle.lengths2<-cycles2[1,][-ncol(cycles2)]
colnames(reconstruction2)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

PrecipSignal <- reconstruction2$signal

#save the signal
write.csv(reconstruction2, "Results/Reconstruction/IsletaPrecipReconstruction.csv", row.names = F)
write.csv(PrecipSignal, "Results/Signal/IsletaPrecipSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function 
PrecipSignal <- as.matrix(read.csv("Results/Signal/IsletaPrecipSignal.csv"))

mutual.out <- mutual(PrecipSignal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
  rownames_to_column() %>% 
  filter(x == min(x)) %>% 
  rename(Emdelay = 1) %>% 
  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(PrecipSignal,m=3,d=d,idt=1,mdt=length(PrecipSignal))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 25 #max number of embedding dimensions to consider
fn.out <- false.nearest(PrecipSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:25])) 
  

#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(PrecipSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/IsletaPrecip_Mx.csv", row.names = F)

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(PrecipSignal)
scatterplot3d(Mx, type = "l", main="shadow precipitation")



#All Data _ San Acacia ####
IsletaPrecip <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R2") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  #filter(between(month(dates), 4, 10)) %>% 
  select(dates, Precip_mm) %>% 
  mutate(time = 1:length(dates)) 

ts <- IsletaPrecip$Precip_mm #change based on what gage using
x.obs <- ts
x<-standardize(x.obs)   #standardize data
dates<-IsletaPrecip$dates

#Fourier Power Spectrum 
# dump("spectral_udf", file="Functions/spectral_udf.R");source("Functions/spectral_udf.R")
# 
# results2<-spectral(x,method_spec="ar")
# 
# # Table of Fourier Results
# cycle_lengths1<-na.omit(unlist(results2)[1:4])
# frequencies1<-1/cycle_lengths1
# fourier_table2<-data.frame(rbind(cycle_lengths1,frequencies1))
# fourier_table3<-t(fourier_table2)
# colnames(fourier_table3)<-c("cycle_lengths","frequencies")
# head(fourier_table3)


#Singular Spectrum Analysis 
dump("SSA_udf", file="Functions/SSA_udf.R");source("Functions/SSA_udf.R")  #SSA

output<-SSA(x)  #run SSA_udf
w.corr2<-output[[1]]
groups2<-output[[2]]
reconstruction2<-output[[3]]
cycles2<-output[[4]]
cycle.lengths2<-cycles2[1,][-ncol(cycles2)]
colnames(reconstruction2)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

PrecipSignal <- reconstruction2$signal

#save the signal
write.csv(reconstruction2, "Results/Reconstruction/SanAcaciaPrecipReconstruction.csv", row.names = F)
write.csv(PrecipSignal, "Results/Signal/SanAcaciaPrecipSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function 
#PrecipSignal <- as.matrix(read.csv("Results/Signal/SanAcaciaPrecipSignal.csv"))

mutual.out <- mutual(PrecipSignal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
                  rownames_to_column() %>% 
                  filter(x == min(x)) %>% 
                  rename(Emdelay = 1) %>% 
                  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(PrecipSignal,m=3,d=d,idt=1,mdt=length(PrecipSignal))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 25 #max number of embedding dimensions to consider
fn.out <- false.nearest(PrecipSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:25])) 


#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(PrecipSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/SanAcaciaPrecip_Mx.csv", row.names = F)

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(PrecipSignal)
scatterplot3d(Mx, type = "l", main="shadow precipitation")

