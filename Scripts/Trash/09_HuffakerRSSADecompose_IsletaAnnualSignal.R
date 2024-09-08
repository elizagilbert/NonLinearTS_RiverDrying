#testing RSSA and Huffaker annual to be same signal ####

library(scatterplot3d) #plot phase-space attractor
library(tseriesChaos) #embed data
library(tidyverse)
library(lubridate)
library(forestmangr)  #round elements in matrix
library(robustHD) #standardize
library(Rssa) #singluar spectrum analysis
library(fpp3) #times series decomposition


#All Data _ Isleta ####
ts <- read.csv("Data/Processed/ExtentChngDry_Irrig.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date) %>%   
  filter(between(month(dates), 4, 10))
x.obs <- ts$ExtentDry
x<-standardize(x.obs)   #standardize data
dates<-ts$dates

ts2 <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date)
x2.obs <- ts2$ExtentDry
x2<-standardize(x2.obs)   #standardize data
dates<-ts$dates


#Huffaker signal ####
dump("SSA_udf", file="Functions/SSA_udf.R");source("Functions/SSA_udf.R")  #SSA

output<-SSA(x)  #run SSA_udf
w.corr<-output[[1]]
groups<-output[[2]]
reconstruction<-output[[3]]
cycles<-output[[4]]
cycle.lengths<-cycles[1,][-ncol(cycles)]
colnames(reconstruction)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

IsletaSignal <- reconstruction$signal

 #test irig versus all year's data
output2<-SSA(x2)  #run SSA_udf
w.corr2<-output2[[1]]
groups2<-output2[[2]]
reconstruction2<-output2[[3]]
cycles2<-output2[[4]]
cycle.lengths2<-cycles2[1,][-ncol(cycles2)]
colnames(reconstruction2)<-c("dates","data","standardized","signal","noise",
                             paste("cycle_",as.integer(cycle.lengths2),sep='')) 

IsletaSignal2 <- reconstruction2$signal

#Rssa ####
#Singular Spectrum Analysis - Rssa ####
#Huffaker function - has errors
isleta_tsobj <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  select(ExtentDry)

ts_data <- ts(isleta_tsobj, start = c(2010, 1), end = c(2021, 12), frequency = 365)


ssa_obj <- ssa(ts_data)

# Summary of SSA results
summary(ssa_obj)

# Plot the singular values to help decide on the number of components
# supposedly you decide on the number of components by seeing an elbow
# so maybe 6 where it goes from a steep decline to a gradual slope
plot(ssa_obj, type = "values")

 #this gives you all the components
plot(ssa_obj, type = "series")


# Reconstruct the components 
reconstructed <- reconstruct(ssa_obj, groups = list(Trend = c(1), Season1 = c(2,3), Season2 = c(4, 5)))
reconstructed2<- reconstruct(ssa_obj, groups = list(1, 1:4, 1:6))

# Plot the reconstructed series
plot(reconstructed, plot.method = "xyplot")
plot(reconstructed2, plot.method = "xyplot")

# Extract the reconstructed series if needed
IsletaSignalRssa <- reconstructed$F1
resid_Is <- reconstructed$F

#decompose ####
extent_isl_ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date) %>%   
  select(dates, ExtentDry) %>% 
  as_tsibble(index = dates)

extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(window = 365),robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()

decomp_is <- extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(), robust = TRUE)) %>% 
  components() %>% 
  select(season_week) 

#random ####
# Decompose 'co2' series with default parameters
s <- ssa(co2)

# Summary of SSA results
summary(s)

# Plot the singular values to help decide on the number of components
plot(s, type = "values")

# Reconstruct the series, grouping elementary series.
r <- reconstruct(s, groups = list(Trend = c(1, 4), Season1 = c(2,3), Season2 = c(5, 6)))
plot(r)
# 'groups' argument might contain duplicate entries as well
r2 <- reconstruct(s, groups = list(1, 1:4, 1:6))
plot(r2)

co2
class(co2)



# Reconstruct the components based on the determined number of significant components
# Example: Let's assume the first 6 components are significant
reconstructed <- reconstruct(ssa_obj, groups = list(Trend = c(1, 2), Season1 = c(3, 4), Season2 = c(5, 6)))

# Calculate the residuals
reconstructed_sum <- rowSums(as.data.frame(reconstructed$F))
residuals <- ts_data - reconstructed_sum

# Plot the reconstructed series
plot(reconstructed, plot.method = "xyplot")


fs <- fossa(ssa_obj, nested.groups = 1:4)
ios <- iossa(fs, nested.groups = list(1:2, 3:4), maxiter = 100)
summary(ios)

plot(ios)

