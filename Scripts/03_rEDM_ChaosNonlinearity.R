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
  filter(Reach == "R2") %>%
  mutate_at(vars(contains(c("Extent", "Discharge_cfs", "Diversion_cfs",
  "Returns_cfs", "Temp_C", "Precip_mm"))), scale1) %>%
  select(Date, Extent, Discharge_cfs, Diversion_cfs,
         Returns_cfs, Temp_C, Precip_mm) %>% 
  rename(date = Date)

#looking for optimal embedding dimension - look for the peak w
starttime <- Sys.time()
E_IslExtent <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                         columns = "Extent", target = "Extent")%>% 
                            filter(rho == max(rho)) %>% 
                            select(E))



E_IslDisch <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Discharge_cfs", target = "Discharge_cfs")%>% 
                           filter(rho == max(rho)) %>% 
                           select(E))


E_IslDiv <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                      columns = "Diversion_cfs", target = "Diversion_cfs")%>% 
                         filter(rho == max(rho)) %>% 
                         select(E))

E_IslRet <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                      columns = "Returns_cfs", target = "Returns_cfs")%>% 
                         filter(rho == max(rho)) %>% 
                         select(E))

E_IslTemp <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                       columns = "Temp_C", target = "Temp_C")%>% 
                          filter(rho == max(rho)) %>% 
                          select(E))

E_IslPrecip <- as.numeric(EmbedDimension(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                         columns = "Precip_mm", target = "Precip_mm")%>% 
                            filter(rho == max(rho)) %>% 
                            select(E))



#assess whether data are nonlinear -if nonlinear then you should see prediction forecast improves as theta
#increases at least initially. 
rho_theta_IsletaExt <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                              columns = "Extent", target = "Extent", E = E_IslExtent)

rho_theta_IsletaDisch <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Discharge_cfs", target = "Discharge_cfs", E = E_IslDisch)

rho_theta_IsletaDiv <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Diversion_cfs", target = "Diversion_cfs", E = E_IslDiv)

rho_theta_IsletaRet <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Returns_cfs", target = "Returns_cfs", E = E_IslRet)

rho_theta_IsletaTemp <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Temp_C", target = "Temp_C", E = E_IslTemp)

rho_theta_IsletaPrecip <- PredictNonlinear(dataFrame = datR1, lib = "1 1825", pred = "2190 3285", 
                                        columns = "Precip_mm", target = "Precip_mm", E = E_IslPrecip)
stoptime <- Sys.time()
runtime <- stoptime -starttime
runtime

dat <- as.data.frame(cbind(rho_theta_IsletaExt$Theta, rho_theta_IsletaExt$rho, rho_theta_IsletaDisch$rho, rho_theta_IsletaDiv$rho,
             rho_theta_IsletaRet$rho, rho_theta_IsletaTemp$rho, rho_theta_IsletaPrecip$rho)) %>% 
  rename(Theta = 1,Ext_rho = 2, Disch_rho = 3, Div_rho = 4, Ret_rho = 5, Temp_rho = 6, Precip_rho = 7)

write.csv(dat, "Results/rEDM/Prediction_SMap/Isleta_Rho_vs_SMap_Theta.csv", row.names = F)
