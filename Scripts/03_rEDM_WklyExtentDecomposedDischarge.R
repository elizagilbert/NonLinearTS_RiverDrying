#read me ####
#The purpose of this script is to experiment with the diversion subreach data
#think about a 3 species coupled system of discharge, diversion, returns

#Libraries ####
library(rEDM)
library(tidyverse)
library(lubridate)
library(fpp3)
library(beepr)
library(dataRetrieval)
library(zoo)

#data ####
#Data ####
extent_isl_ts <- read.csv("Data/Processed/ExtentChngDry.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date) %>%   
  select(dates, ExtentDry) %>% 
  as_tsibble(index = dates)

discharge_is_ts <- readNWISdv(siteNumbers = "08330875",
                        parameterCd = "00060",
                        startDate = "2010-01-01",
                        endDate = "2021-12-31") %>% 
  rename(IsletaDisch = 4, dates = Date) %>% 
  complete(dates = seq(min(dates), max(dates), by = "day")) %>%
  mutate(IsletaDisch = na.approx(IsletaDisch)) %>% 
  as_tsibble(index = dates) 

discharge_bosq_ts <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010)%>% 
  rename(BosqueDisch = 4) %>% 
  as_tsibble(index = dates)

#STL decomposition with trend and season ####
#plots
extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(),robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()

discharge_is_ts %>% 
  model(STL(IsletaDisch ~ trend() + season(),robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()

discharge_bosq_ts %>% 
  model(STL(BosqueDisch ~ trend() + season(),robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()


#selecting signal of week
decomp_bosq_disch <- discharge_bosq_ts %>% 
  model(STL(BosqueDisch ~ trend() + season(), robust = TRUE)) %>% 
  components() %>% 
  select(season_week) %>% 
  rename(BosqueDisch = season_week) %>% 
  filter(between(month(dates), 4,10))


decomp_islet_week <- extent_isl_ts %>% 
  model(STL(ExtentDry ~ trend() + season(), robust = TRUE)) %>% 
  components() %>% 
  select(season_week) %>% 
  rename(Extent = season_week) %>% 
  filter(between(month(dates), 4,10))


dat_all <- bind_cols(decomp_islet_week, decomp_bosq_disch) %>% 
  rename(date = 2, BosqueDisch = 3, date2 = 4) %>% 
  select(date, Extent, BosqueDisch)


#start EDM ####
simplex_out1 <- Simplex(dataFrame = dat_all$Extent, lib = "1 1000", pred = "1200 2500", 
                       columns = "Extent", target = "Extent", E = 3 )


ComputeError(simplex_out1$Observations, simplex_out1$Predictions)


#looking for optimal embedding dimension - look for the peak which for Isleta week decomp is 7
rho_E_R1 <- EmbedDimension(dataFrame = dat_all, lib = "4 1000", pred = "1200 2500", 
                        columns = "Extent", target = "Extent")

E1 = 7

#reduction in forecast means the system could be chaotic 
rho_Tp_R1 <- PredictInterval(dataFrame = dat_all, lib = "4 1000", pred = "1200 2500", 
                          columns = "Extent", target = "Extent", E = E1)



#assess whether data are nonlinear -if nonlinear then you should see prediction forecast improve as theta (x-axis)
#increases 
rho_theta_R1 <- PredictNonlinear(dataFrame = dat_all, lib = "1 1000", pred = "1200 2500", 
                              columns = "Extent", target = "Extent", E = E1)



#convergent cross mapping
#cross mapping indicates causal influence in the reverse direction 
#target to source meaning for blue Isleta Discharge influences Extent dry
#and red is Extent influences Isleta Discharge - The weekly makes more
#sense as the blue has a higher prediction than the red for Isleta Lakes 
#causing Isleta drying same result for Bosque

start.time <- Sys.time()

(cor_R1 <- cor(dat_all$BosqueDisch, dat_all$Extent))
(cor_R1_1 <- cor(dat_all$BosqueDisch, dat_all$Extent))
cmap_R1 <- CCM(dataFrame = dat_all, E = E1, Tp = 0, columns = "Extent",
               target = "BosqueDisch", libSizes = "13 90 3", sample = 300, showPlot = T)


end.time <- Sys.time()
print(round(end.time - start.time,2)) 
beep(3)

write.csv(cmap_R1, "Results/CCM/cmap_Extent_Isleta_BosqueDischDisch_Wkly.csv")

#CCM ####
start.time <- Sys.time()

vars = colnames(dat_all[2:4])
var_pairs = combn(vars, 2) # Combinations of vars, 2 at a time
libSize = paste(NROW(dat_all) - E1, NROW(dat_all) - E1, 10, collapse = " ")
ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, vars))

for (i in 1:ncol(var_pairs)) {
  ccm_out = CCM(dataFrame = dat_all, columns = var_pairs[1, i], target = var_pairs[2,
                i], libSizes = libSize, Tp = 0, E = E1, sample = 100)
  outVars = names(ccm_out)
  var_out = unlist(strsplit(outVars[2], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
  var_out = unlist(strsplit(outVars[3], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
}


write.csv(ccm_matrix, "Results/CCM/ccm_matrix_IsletaExtDischarge_Wkly.csv")

#compute the lagged cross-correlation allowing lags up to +/- 7 days
corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(dat_all[, ccm_from], dat_all[, ccm_to], type = "correlation",
                   lag.max = 7, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out))
  }
}

write.csv(corr_matrix, "Results/CCM/corr_matrix_IsletaExtDischarge_Wkly.csv")


#examine CCM predictability
par(mfrow = c(2,1))

ext_xmap_maxT <- CCM(dataFrame = dat_all, E = E1, Tp = 0, columns = "Extent",
                        target = "IsletaDisch", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Extent", "IsletaDisch"], col = "black", lty = 2)

ext_xmap_maxT <- CCM(dataFrame = dat_all, E = E1, Tp = 0, columns = "Extent",
                        target = "BosqueDisch", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Extent", "BosqueDisch"], col = "black", lty = 2)


end.time <- Sys.time()
print(round(end.time - start.time,2)) #14 minutes Isleta and 2 discharge gages
beep(3)