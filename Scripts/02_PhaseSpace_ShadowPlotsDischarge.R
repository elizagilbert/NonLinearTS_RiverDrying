library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(zoo)
library(dataRetrieval)


#All Data _ Isleta ####
BosqueFarms <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) #%>%
  #filter(between(month(dates), 4, 10))

# SanAcacia <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
#   filter(site_name == "San Acacia") %>% 
#   mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
#   filter(year(dates) >= 2010) %>% 
#   filter(between(month(dates), 4, 10)) %>% 
#   mutate(Discharge_cfs = na.approx(Discharge_cfs))
# 
# IsletaDis <- readNWISdv(siteNumbers = "08330875",
#                      parameterCd = "00060",
#                      startDate = "2010-01-01",
#                      endDate = "2021-12-31") %>% 
#   rename(Discharge_cfs = 4, dates = Date) %>% 
#   complete(dates = seq(min(dates), max(dates), by = "day")) %>%
#   mutate(Discharge_cfs = na.approx(Discharge_cfs)) %>% 
#   filter(between(month(dates), 4, 10))

ts <- BosqueFarms #change based on what gage using
x.obs <- ts$Discharge_cfs
x<-standardize(x.obs)   #standardize data
dates<-ts$dates

#Fourier Power Spectrum ####
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

#save the signal
write.csv(reconstruction2, "Results/Reconstruction/BosqueReconstruction.csv", row.names = F)
write.csv(DischargeSignal, "Results/Signal/BosqueDischSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function ####
DischargeSignal <- as.matrix(read.csv("Results/Signal/BosqueDischSignal.csv"))

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
m.max <- 10 #max number of embedding dimensions to consider
fn.out <- false.nearest(DischargeSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 
  

#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(DischargeSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/BosqueDischarge_Mx.csv", row.names = F)
#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(DischargeSignal)
scatterplot3d(Mx, type = "l", main="shadow discharge")



#All Data _ San Acacia ####
Escondida <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "Escondida") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  mutate(Discharge_cfs = na.approx(Discharge_cfs, na.rm = F))
#filter(between(month(dates), 4, 10))

ts <- Escondida #change based on what gage using
x.obs <- ts$Discharge_cfs
x<-standardize(x.obs)   #standardize data
dates<-ts$dates

#Fourier Power Spectrum ####
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

#save the signal
write.csv(reconstruction2, "Results/Reconstruction/EscondidaReconstruction.csv", row.names = F)
write.csv(DischargeSignal, "Results/Signal/EscondidaDischSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function ####
#DischargeSignal <- as.matrix(read.csv("Results/Signal/EscondidaDischSignal.csv"))

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
m.max <- 10 #max number of embedding dimensions to consider
fn.out <- false.nearest(DischargeSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 


#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(DischargeSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/EscondidaDischarge_Mx.csv", row.names = F)
#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(DischargeSignal)
scatterplot3d(Mx, type = "l", main="shadow discharge")

