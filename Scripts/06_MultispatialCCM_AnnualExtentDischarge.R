#read me ####
#The purpose of this script is to experiment with the diversion subreach data
#think about a 3 species coupled system of discharge, diversion, returns

#Libraries ####
library(rEDM)
library(tidyverse)
library(lubridate)
library(beepr)
library(multispatialCCM)
library(future)
library(zoo)

plan(multisession)

#data ####

#need diversion/returns, extent/precip, extent/temp, precip/discharge

# from other EDM testing and keeping to get Date column
scale1 <- function(x) scale(x)[,1]

BosqueFarms <- read.csv("Data/Processed/USGS_discharge.csv") %>%
  filter(site_name == "BosqueFarms") %>%
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>%
  filter(year(dates) >= 2010)

# Escondida <- read.csv("Data/Processed/USGS_discharge.csv") %>%
#   filter(site_name == "Escondida") %>%
#   mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>%
#   filter(year(dates) >= 2010) %>%
#   mutate(Discharge_cfs = na.approx(Discharge_cfs, na.rm = F))

datR1 <- read.csv("Data/Processed/DiversionSubreachData.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(year(Date) >=2010) %>%
  filter(Reach == "R1") %>%
  cbind(BosqueFarms$Discharge_cfs) %>% 
  mutate_at(vars(contains(c("Extent", "Discharge_cfs", "Diversion_cfs",
                            "Returns_cfs", "Temp_C", "Precip_mm"))), scale1) %>%
  select(Date, Extent, Discharge_cfs, Diversion_cfs, 
         Returns_cfs, Temp_C, Precip_mm)

# # Only April to October, with NA row after each year's October data
# Dat_W_NA <- read.csv("Data/Processed/DiversionSubreachData.csv") %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
#   filter(year(Date) >=2010) %>%
#   filter(Reach == "R1") %>%
#   cbind(BosqueFarms$Discharge_cfs) %>% 
#   mutate_at(vars(contains(c("Extent", "Discharge_cfs", "Diversion_cfs",
#                             "Returns_cfs", "Temp_C", "Precip_mm"))), scale1) %>%
#   select(Date, Extent, Discharge_cfs, Diversion_cfs, 
#          Returns_cfs, Temp_C, Precip_mm) %>% 
#   filter(between(month(Date), 4, 10))%>%
#   mutate(Year = year(Date)) %>% 
#   arrange(Date) %>%
#   group_by(Year) %>%
#   group_modify(~ add_row(.x, Date = NA, Extent = NA, Diversion_cfs = NA, Discharge_cfs = NA, Returns_cfs = NA, Temp_C = NA, Precip_mm = NA)) %>%
#   ungroup() %>%
#   select(-Year)

#Calculate optimal E ####
maxE<-10 #Maximum E to test

#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]<-SSR_pred_boot(A=datR1$Extent, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=datR1$Returns_cfs, E=E, predstep=1, tau=1)$rho
}

#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")

E_A<-6
E_B<-5

#check nonlinear ####
#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
signal_A_out<-SSR_check_signal(A=datR1$Precip_mm, E=E_A, tau=1,
                               predsteplist=1:10)
signal_B_out<-SSR_check_signal(A=datR1$Returns_cfs, E=E_B, tau=1,
                               predsteplist=1:10)
plot(signal_A_out$predatout$predstep, signal_A_out$predatout$rho)
plot(signal_B_out$predatout$predstep, signal_B_out$predatout$rho)

#from rEDM
sys.start <- Sys.time()
rho_theta_R1 <- PredictNonlinear(dataFrame = datR1, lib = "1 1000", pred = "2001 3001", 
                                 columns = "Extent", target = "Extent", E = 2) #a few minutes
sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
run_time
beep(3)

#CCM ####
#Does A "cause" B? #
#CMM_boot is from multispatial CCM

sys.start <- Sys.time()

# Does A "cause" B?
# The first data input is A

  #Note - increase iterations to 100 for consistant results
CCM_boot_A<-CCM_boot(datR1$Extent, datR1$Returns_cfs, E_A, tau=1, iterations=10)


# Does B "cause" A?

CCM_boot_B<-CCM_boot(datR1$Returns_cfs, datR1$Extent, E_B, tau=1, iterations=10)


#Test for significant causal signal
#See R function for details
(CCM_significance_test<-ccmtest(CCM_boot_A,
                                CCM_boot_B))

sys.stop <- Sys.time()
run_time <- sys.stop-sys.start
run_time #20 minutes

beep(1)

CCM_Extent_Discharge <- c(CCM_boot_A, CCM_boot_B)

names(CCM_Extent_Discharge)[4] <- "rho_ExtentCausesReturns_cfs"
names(CCM_Extent_Discharge)[13] <- "rho_Returns_cfsCausesExtent"

saveRDS(CCM_Extent_Discharge, file="Results/MultiSpatial_CCM/Isleta/AllDat_CCMBoot_Extent_Returns_IS.RData")



#Get results ####
temp <- readRDS("Results/MultiSpatial_CCM/Isleta/CCMBoot_Diversions_DischargeIS.RData")

names(temp)[4] <- "rho"
names(temp)[13] <- "rho"

CCM_boot_A <- temp[1:9]
CCM_boot_B <- temp[10:18]

(CCM_significance_test<-ccmtest(CCM_boot_A, CCM_boot_B))

Rho_A <- as.data.frame(CCM_boot_A$rho)
Rho_B <- as.data.frame(CCM_boot_B$rho)

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
#Plot "A causes B"
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)

#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2)
legend("topleft",
       c("A causes B", "B causes A"),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n")
