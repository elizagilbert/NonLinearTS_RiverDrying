library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(plotly)


#All Data _ Isleta ####
ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date)#%>%   
  #filter(between(month(dates), 4, 10))
x.obs <- ts$ExtentDry
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

IsletaSignal <- reconstruction2$signal


#save the signal
write.csv(reconstruction2, "Results/Reconstruction/IsletaExtentReconstruction.csv", row.names = F)
write.csv(IsletaSignal, "Results/Signal/IsletaExtentSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function ####
Signal <- as.matrix(read.csv("Results/Signal/IsletaExtentSignal.csv"))

mutual.out <- mutual(Signal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
  rownames_to_column() %>% 
  filter(x == min(x)) %>% 
  rename(Emdelay = 1) %>% 
  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(Signal,m=3,d=d,idt=1,mdt=length(Signal))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 10 #max number of embedding dimensions to consider
fn.out <- false.nearest(Signal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 
  

#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(Signal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/IsletaExtent_Mx.csv", row.names = F)

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(Signal)

jpeg("Figures/Shadow_ExtentIsleta.jpg", width = 800, height = 600, res = 100)
s3d <- scatterplot3d(Mx, type = "l", main="Shadow Extent",
              xlab = "t",   # Title for the X axis
              ylab = "",   # Title for the Y axis
              zlab = "t+40", 
              lwd = 2, 
              col.axis = "black")

# Manually add the Y-axis label
text(x = s3d$xyz.convert(4, max(Mx[,2]), 0)$x,  # X coordinate for the label
     y = s3d$xyz.convert(0, max(Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+20", 
     srt = 36,               # Adjust the angle of the text
     adj = 6,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 1.)              #

dev.off()




#All Data _ San Acacia ####
ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "San Acacia") %>%  #used year only to see SSA
  rename(dates = Date)#%>%   
#filter(between(month(dates), 4, 10))
x.obs <- ts$ExtentDry
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

SanAcaciaSignal <- reconstruction2$signal


#save the signal
write.csv(reconstruction2, "Results/Reconstruction/SanAcaciaExtentReconstruction.csv", row.names = F)
write.csv(SanAcaciaSignal, "Results/Signal/SanAcaciaExtentSignal.csv", row.names = F)

#Embedding delay with Mutual Information Function ####
#Signal <- as.matrix(read.csv("Results/Signal/SanAcaciaExtentSignal.csv.csv"))

mutual.out <- mutual(SanAcaciaSignal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
                  rownames_to_column() %>% 
                  filter(x == min(x)) %>% 
                  rename(Emdelay = 1) %>% 
                  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(SanAcaciaSignal,m=3,d=d,idt=1,mdt=length(SanAcaciaSignal))


## Isolate observations of highest contour
contour_10<-out[10,1:400]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 10 #max number of embedding dimensions to consider
fn.out <- false.nearest(SanAcaciaSignal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 


#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- for_m
Mx <- embedd(SanAcaciaSignal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]

write.csv(Mx, "Results/Mx/SanAcaciaExtent_Mx.csv", row.names = F)

#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(SanAcaciaSignal)

jpeg("Figures/Shadow_ExtentIsleta.jpg", width = 800, height = 600, res = 100)
s3d <- scatterplot3d(Mx, type = "l", main="Shadow Extent",
                     xlab = "t",   # Title for the X axis
                     ylab = "",   # Title for the Y axis
                     zlab = "t+40", 
                     lwd = 2, 
                     col.axis = "black")

# Manually add the Y-axis label
text(x = s3d$xyz.convert(4, max(Mx[,2]), 0)$x,  # X coordinate for the label
     y = s3d$xyz.convert(0, max(Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+20", 
     srt = 36,               # Adjust the angle of the text
     adj = 6,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 1.)              #

dev.off()




#####
#Analyses aggregated to weeks ####
ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Reach == "Isleta") %>%  # used year only to see SSA
  rename(dates = Date) %>%
  # Add a new column for the week (using lubridate)
  mutate(week = floor_date(dates, unit = "week")) %>%
  # Group by week and calculate the total of ExtentDry for each week
  group_by(week) %>%
  summarise(ExtentDry = sum(ExtentDry, na.rm = TRUE))

plot(ts$ExtentDry)

x.obs <- ts$ExtentDry
x<-standardize(x.obs)   #standardize data
dates<-ts$week

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

Signal <- reconstruction2$signal


#save the signal

#Embedding delay with Mutual Information Function ####

mutual.out <- mutual(Signal, lag.max = 100) #mutual(tseriesChaos) Embedding delay = d 
d <- as.numeric(as.data.frame(mutual.out) %>% 
                  rownames_to_column() %>% 
                  filter(x == min(x)) %>% 
                  rename(Emdelay = 1) %>% 
                  select(Emdelay))

# dump("embed_delay_udf", file="Functions/embed_delay_udf.R");source("Functions/embed_delay_udf.R")
# d<-d_udf(IsletaSignal)  #compute average mutual information function with udf embed_delay_udf

par(mfrow=c(1,2))  
out<-stplot(Signal,m=3,d=d,idt=1,mdt=length(Signal))


## Isolate observations of highest contour
contour_10<-out[10,1:200]
plot(contour_10,type='l')

#false nearest neighobors test
#embedding parameter is d
tw <- as.numeric(which.max(contour_10))


#from tseriesChaos
m.max <- 6 #max number of embedding dimensions to consider
fn.out <- false.nearest(Signal, m.max, d, tw)
for_m <- as.numeric(which.min(fn.out[2, 1:10])) 


#to plot
fn.out[is.na(fn.out)] <- 0
plot(fn.out) #shows best embedding which is where % false nearest neighbors drops to lowest value



#Time-delay embedding
m <- 4
Mx <- embedd(Signal, m=m, d=d)
head(Mx)

Mx <- Mx[,1:3]


#Plotting shadow and phase-space together

#user-defined function for time-delay embedding
embedding <- function(x){
  Mx <- embedd(x,2,1)
  plot(Mx[,1], Mx[,2], type = "l", main="phase-space plot")
}

par(mfrow=c(1,1)) 
embedding(Signal)

s3d <- scatterplot3d(Mx, type = "l", main="Shadow Extent",
                     xlab = "t",   # Title for the X axis
                     ylab = "",   # Title for the Y axis
                     zlab = "t+40", 
                     lwd = 2, 
                     col.axis = "black")

# Manually add the Y-axis label
text(x = s3d$xyz.convert(4, max(Mx[,2]), 0)$x,  # X coordinate for the label
     y = s3d$xyz.convert(0, max(Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+20", 
     srt = 36,               # Adjust the angle of the text
     adj = 6,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 1.)              #

dev.off()



