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
  rename(Date = 2) %>% 
  select(Date, Extent)

dat_DischSigIsleta <- read.csv("Results/Signal_Discharge/IsletaDischargeSignal_Irrig.csv") %>% 
  rename(IsletaDisch = 1)

dat_DischSigBosque <- read.csv("Results/Signal_Discharge/BosqueFarmsSignal_Irrig.csv") %>% 
  rename(BosqueDisch = 1)

dat_all <- cbind(IsletaSign, dat_DischSigBosque, dat_DischSigIsleta)

#start EDM ####
simplex_out1 <- Simplex(dataFrame = IsletaSign, lib = "1 1000", pred = "2000 2500", 
                       columns = "Extent", target = "Extent", E = 3 )


ComputeError(simplex_out1$Observations, simplex_out1$Predictions)


#looking for optimal embedding dimension - look for the peak which for Isleta Irrig is 2
rho_E_R1 <- EmbedDimension(dataFrame = IsletaSign, lib = "4 1000", pred = "2001 2500", 
                        columns = "Extent", target = "Extent")


#reduction in forecast means the system could be chaotic 
rho_Tp_R1 <- PredictInterval(dataFrame = IsletaSign, lib = "4 1000", pred = "2001 2500", 
                          columns = "Extent", target = "Extent", E = 2)



#assess whether data are nonlinear -if nonlinear then you should see prediction forecast improves as theta
#increases so this would suggest Isleta is not nonlinear. However, these data are signal without the
#noise so I'm not sure this is the rigth test and would lean more heavily on Huffaker surrogate
#for the test of nonlinearity deterministic.
rho_theta_R1 <- PredictNonlinear(dataFrame = IsletaSign, lib = "1 1000", pred = "2001 2500", 
                              columns = "Extent", target = "Extent", E = 2)



#convergent cross mapping
#cross mapping indicates causal influence in the reverse direction 
#target to source meaning for blue Isleta Discharge influences Extent dry
#and red is Extent influences Isleta Discharge - which doesn't make any sense
#given discharge is upstream of drying.

start.time <- Sys.time()

(cor_R1 <- cor(dat_all$BosqueDisch, dat_all$Extent))
(cor_R1_1 <- cor(dat_all$BosqueDisch, dat_all$Extent))
cmap_R1 <- CCM(dataFrame = dat_all, E = 2, Tp = 0, columns = "Extent",
               target = "BosqueDisch", libSizes = "13 73 3", sample = 300, showPlot = T)


end.time <- Sys.time()
print(round(end.time - start.time,2)) #only 1.42 minutes for Isleta Annual Ext and Isleta Discharge
beep(3)

write.csv(cmap_R1, "Results/CCM/cmap_Extent_IsletaBosqueDisch_Annul.csv")

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

#compute the lagged cross-correlation allowing lags up to +/- 2 years
corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(dat_all[, ccm_from], dat_all[, ccm_to], type = "correlation",
                   lag.max = 10, plot = FALSE)$acf
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

