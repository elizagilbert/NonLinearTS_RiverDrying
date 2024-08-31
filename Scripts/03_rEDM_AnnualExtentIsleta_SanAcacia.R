#read me ####
#The purpose of this script is to experiment with the diversion subreach data
#think about a 3 species coupled system of discharge, diversion, returns

#Libraries ####
library(rEDM)
library(tidyverse)
library(lubridate)
library(beepr)

#data ####

# from other EDM testing and keeping to get Date column
scale1 <- function(x) scale(x)[,1]

datR1 <- read.csv("Data/Processed/DiversionSubreachData.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(year(Date) >=2010) %>%
  filter(Reach == "R1") %>%
  filter(between(month(Date), 4, 10)) %>%
  mutate_at(vars(contains(c("Extent", "Discharge_cfs", "Diversion_cfs",
  "Returns_cfs", "Temp_C", "Precip_mm"))), scale1) %>%
  select(Date, Extent, Discharge_cfs, Diversion_cfs,
         Returns_cfs, Temp_C, Precip_mm)

#processed signal from Huffaker
#for rEDM have to have a date column and it needs to be before the variable
IsletaSign <- read.csv("Results/IsletaSignal_Irrig.csv") %>% 
  rename(Extent = x) %>% 
  mutate(Extent = as.numeric(Extent)) %>% 
  cbind(datR1$Date) %>% 
  rename(Date = 2, ExtentIsleta = Extent) %>% 
  select(Date, ExtentIsleta)

SanAcaciaSign <- read.csv("Results/SanAcaciaSignal_Irrig.csv") %>% 
  rename(Extent = x) %>% 
  mutate(Extent = as.numeric(Extent)) %>% 
  cbind(datR1$Date) %>% 
  rename(Date = 2, ExtentSanAcacia = Extent) %>% 
  select(Date, ExtentSanAcacia)

IsletaExtent <- read.csv("Data/Processed/ExtentChngDry_Irrig.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "Isleta") %>%  #used year only to see SSA
  rename(dates = Date, ExtentIsleta_Raw = ExtentDry) %>%   
  filter(between(month(dates), 4, 10))

SanAcaciaExtent <- read.csv("Data/Processed/ExtentChngDry_Irrig.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Reach == "San Acacia") %>%  #used year only to see SSA
  rename(dates = Date, ExtentSanAcacia_Raw = ExtentDry) %>%   
  filter(between(month(dates), 4, 10))

dat_all <- cbind(IsletaSign, SanAcaciaSign) %>% 
  select(-3)

#start EDM ####
simplex_Isleta <- Simplex(dataFrame = IsletaSign, lib = "1 1000", pred = "2000 2500", 
                       columns = "ExtentIsleta", target = "ExtentIsleta", E = 3 )

simplex_SanAcacia <- Simplex(dataFrame = SanAcaciaSign, lib = "1 1000", pred = "2000 2500", 
                          columns = "ExtentSanAcacia", target = "ExtentSanAcacia", E = 3 )

ComputeError(simplex_Isleta$Observations, simplex_Isleta$Predictions)
ComputeError(simplex_SanAcacia$Observations, simplex_SanAcacia$Predictions)

#looking for optimal embedding dimension - look for the peak which 2 for both river reaches
rho_E_Isleta <- EmbedDimension(dataFrame = IsletaSign, lib = "4 1000", pred = "2001 2500", 
                        columns = "ExtentIsleta", target = "ExtentIsleta")

rho_E_SanAcacia <- EmbedDimension(dataFrame = SanAcaciaSign, lib = "4 1000", pred = "2001 2500", 
                               columns = "ExtentSanAcacia", target = "ExtentSanAcacia")

E = 2


rho_E_IsletaRaw <- EmbedDimension(dataFrame = IsletaExtent, lib = "4 1000", pred = "2001 2500", 
                               columns = "ExtentIsleta_Raw", target = "ExtentIsleta_Raw")
Eraw_isleta = 3

rho_E_SanAcaciaRaw <- EmbedDimension(dataFrame = SanAcaciaExtent, lib = "4 1000", pred = "2001 2500", 
                                  columns = "ExtentSanAcacia_Raw", target = "ExtentSanAcacia_Raw")
Eraw_sanacacia = 5

#reduction in forecast means the system could be chaotic 
rho_Tp_Isleta <- PredictInterval(dataFrame = IsletaSign, lib = "4 1000", pred = "2001 2500", 
                          columns = "ExtentIsleta", target = "ExtentIsleta", E = E)

rho_Tp_SanAcacia <- PredictInterval(dataFrame = SanAcaciaSign, lib = "4 1000", pred = "2001 2500", 
                             columns = "ExtentSanAcacia", target = "ExtentSanAcacia", E = E)



rho_Tp_IsletaRaw <- PredictInterval(dataFrame = IsletaExtent, lib = "4 1000", pred = "2001 2500", 
                                 columns = "ExtentIsleta_Raw", target = "ExtentIsleta_Raw", E = Eraw_isleta)

rho_Tp_SanAcaciaRaw <- PredictInterval(dataFrame = SanAcaciaExtent, lib = "4 1000", pred = "2001 2500", 
                                    columns = "ExtentSanAcacia_Raw", target = "ExtentSanAcacia_Raw", E = Eraw_sanacacia)

#assess whether data are nonlinear -if nonlinear then you should see prediction forecast improves as theta
#increases. 
#Decreases with signal data for both reaches. Increase with raw for isleta then decreases and decreases with raw
#for San Acacia

#However, Huffaker surrogate tests for nonlinear deterministic, not just nonlinear.
rho_theta_Isleta <- PredictNonlinear(dataFrame = IsletaSign, lib = "1 1000", pred = "2001 2500", 
                              columns = "ExtentIsleta", target = "ExtentIsleta", E = E) #decreases with theta

rho_theta_SanAcacia <- PredictNonlinear(dataFrame = SanAcaciaSign, lib = "1 1000", pred = "2001 2500", 
                                 columns = "ExtentSanAcacia", target = "ExtentSanAcacia", E = E)


rho_theta_IsletaRaw <- PredictNonlinear(dataFrame = IsletaExtent, lib = "4 1000", pred = "2001 2500", 
                                    columns = "ExtentIsleta_Raw", target = "ExtentIsleta_Raw", E = Eraw_isleta)

rho_theta_SanAcaciaRaw <- PredictNonlinear(dataFrame = SanAcaciaExtent, lib = "4 1000", pred = "2001 2500", 
                                       columns = "ExtentSanAcacia_Raw", target = "ExtentSanAcacia_Raw", E = Eraw_sanacacia)

#convergent cross mapping
#cross mapping indicates causal influence in the reverse direction 
#target to source meaning for blue Isleta Discharge influences Extent dry
#and red is Extent influences Isleta Discharge - which doesn't make any sense
#given discharge is upstream of drying.

start.time <- Sys.time()

(cor_R1 <- cor(dat_all$ExtentIsleta, dat_all$ExtentSanAcacia)) 
cmap_R1 <- CCM(dataFrame = dat_all, E = 2, Tp = 0, columns = "ExtentSanAcacia",
               target = "ExtentIsleta", libSizes = "13 73 3", sample = 300, showPlot = T) #does Isleta cause SanAcacia


end.time <- Sys.time()
print(round(end.time - start.time,2)) #only 1.42 minutes for Isleta Annual Ext and Isleta Discharge
beep(1)

write.csv(cmap_R1, "Results/CCM/cmap_ExtentSignal_IsletaSanAcacia_IrrigAnnual.csv")

#CCM ####
#Reach 1 ####
start.time <- Sys.time()

E1 = 2
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
end.time <- Sys.time()
print(round(end.time - start.time,2)) #14 minutes Isleta and 2 discharge gages
beep(3)

write.csv(ccm_matrix, "Results/CCM/ccm_matrix_IsletaExtDischarge.csv")

#compute the lagged cross-correlation allowing lags up to +/- 7 days (I think it is still in days)
corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(dat_all[, ccm_from], dat_all[, ccm_to], type = "correlation",
                   lag.max = 7, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out))
  }
}

write.csv(corr_matrix, "Results/CCM/corr_matrix_IsletaExtDischarge.csv")

#examine CCM predictability
par(mfrow = c(2,1))

ext_xmap_maxT <- CCM(dataFrame = dat_all, E = E1, Tp = 0, columns = "Extent",
                        target = "IsletaDisch", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Extent", "IsletaDisch"], col = "black", lty = 2)

ext_xmap_maxT <- CCM(dataFrame = dat_all, E = E1, Tp = 0, columns = "Extent",
                        target = "BosqueDisch", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Extent", "BosqueDisch"], col = "black", lty = 2)
beep(3)

