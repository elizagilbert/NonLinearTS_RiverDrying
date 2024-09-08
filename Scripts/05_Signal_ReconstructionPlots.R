library(tidyverse)
library(cowplot)

# Isleta ####
Recon_IsletaExt <- read.csv("Results/Reconstruction/IsletaExtentReconstruction.csv")
Recon_BosqDisch <- read.csv("Results/Reconstruction/BosqueReconstruction.csv")
Recon_IsletaDiv <- read.csv("Results/Reconstruction/IsletaDiversionReconstruction.csv")
Recon_IsletaRet <- read.csv("Results/Reconstruction/IsletaReturnReconstruction.csv")
Recon_IsletaTemp <- read.csv("Results/Reconstruction/IsletaTempReconstruction.csv")
Recon_IsletaPrecip <- read.csv("Results/Reconstruction/IsletaPrecipReconstruction.csv")

dat<- as.data.frame(cbind(Recon_IsletaExt$dates, 
                    Recon_IsletaExt$standardized, Recon_IsletaExt$signal,
                    Recon_BosqDisch$standardized, Recon_BosqDisch$signal,
                    Recon_IsletaDiv$standardized, Recon_IsletaDiv$signal,
                    Recon_IsletaRet$standardized, Recon_IsletaRet$signal,
                    Recon_IsletaTemp$standardized, Recon_IsletaTemp$signal,
                    Recon_IsletaPrecip$standardized, Recon_IsletaPrecip$signal))
dat_isleta <- dat %>% 
  rename(Dates = 1,
         Extent_observed = 2, Extent_signal = 3,
         Discharge_observed = 4, Discharge_signal = 5,
         Diversion_observed = 6, Diversion_signal = 7,
         Returns_observed = 8, Returns_signal = 9,
         Temperature_observed = 10, Temperature_signal = 11,
         Precipitation_observed = 12, Precipitation_signal = 13) %>% 
  mutate(Dates = as.Date(Dates, format = "%Y-%m-%d")) 

#Plotting data and signal
df_long <- dat_isleta %>%
  pivot_longer(cols = -Dates,  # Everything except 'Date' should be pivoted
               names_to = c("Variable", "Type"),  # Separate into 'Variable' and 'Type'
               names_sep = "_") %>%
  mutate(Variable = factor(Variable, levels = c("Extent", "Discharge", "Diversion", "Returns", 
                                                "Precipitation", "Temperature"))) %>% 
  mutate(Variable = fct_recode(Variable, "Extent dry" = "Extent"))

jpeg("Figures/IsletaObservedSignal.jpg", width = 800, height =500, res = 100)
ggplot(df_long, aes(x = Dates, y = value, color = Type)) +
  geom_line() +
  facet_wrap(vars(Variable)) +
  scale_color_manual(values = c("observed" = "black", "signal" = "red")) +  # Set colors manually
  labs(title = "Isleta reach", x = "", y = "scaled value", color = "") +
  theme_classic()+
  theme(legend.position = c(0.12, 0.94),
        legend.direction = "horizontal",  # Arrange legend items in a single row
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA))
dev.off()

#Plotting data and components 

Pl_Extent <- Recon_IsletaExt %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_1963, cycle_NA), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta extent dry", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_1963" = "#D55E00", 
                                "cycle_NA" = "#56B4E9"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day ", 
                                "cycle_1963" = "1963-day ", 
                                "cycle_NA" = "unknown"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 


Pl_IsletaDiv <- Recon_IsletaDiv %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_182, cycle_91, cycle_73, cycle_122), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta dam diversion", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_182" = "#D55E00", 
                                "cycle_91" = "#56B4E9",
                                "cycle_73" = "#F0E442",
                                "cycle_122" = "#009E73"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day", 
                                "cycle_182" = "182-day", 
                                "cycle_91" = "91-day",
                                "cycle_73" = "73-day",
                                "cycle_122" = "122-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 

Pl_BosqDisch <- Recon_BosqDisch %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_709, cycle_406, cycle_1407), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Bosque Farms discharge", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_709" = "#0072B2", 
                                "cycle_406" = "#D55E00", 
                                "cycle_1407" = "#56B4E9"),
                     labels = c("standardized" = "ts", 
                                "cycle_709" = "709-day", 
                                "cycle_406" = "406-day", 
                                "cycle_1407" = "1407-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 


Pl_IsletaReturn <- Recon_IsletaRet %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_361, cycle_184, cycle_777, cycle_254), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach returns", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_361" = "#0072B2", 
                                "cycle_184" = "#D55E00", 
                                "cycle_777" = "#56B4E9",
                                "cycle_254" = "#F0E442"),
                     labels = c("standardized" = "ts", 
                                "cycle_361" = "361-day", 
                                "cycle_184" = "184-day", 
                                "cycle_777" = "777-day",
                                "cycle_254" = "254-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 

Pl_IsletaTemp <- Recon_IsletaTemp %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_181, cycle_121, cycle_74), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach temperature", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_181" = "#D55E00", 
                                "cycle_121" = "#56B4E9",
                                "cycle_74" = "#F0E442"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day", 
                                "cycle_181" = "181-day", 
                                "cycle_121" = "121-day",
                                "cycle_74" = "74-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 



Pl_IsletaPrecip <- Recon_IsletaPrecip %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_364, cycle_57, cycle_25, cycle_17, cycle_NA), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach precipitation", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_364" = "#0072B2", 
                                "cycle_57" = "#D55E00", 
                                "cycle_25" = "#56B4E9",
                                "cycle_17" = "#F0E442",
                                "cycle_NA" = "#CC79A7"),
                     labels = c("standardized" = "ts", 
                                "cycle_364" = "364-day", 
                                "cycle_57" = "57-day", 
                                "cycle_25" = "25-day",
                                "cycle_17" = "17-day",
                                "cycle_NA" = "unknown"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3))  

jpeg("Figures/IsletaReconstruction.jpg", width = 800, height =990, res = 100)

plot_grid(Pl_Extent, Pl_BosqDisch, Pl_IsletaDiv, Pl_IsletaReturn, Pl_IsletaTemp, Pl_IsletaPrecip,
          ncol = 2, nrow = 3)

dev.off()


# San Acacia ####
Recon_IsletaExt <- read.csv("Results/Reconstruction/IsletaExtentReconstruction.csv")
Recon_BosqDisch <- read.csv("Results/Reconstruction/BosqueReconstruction.csv")
Recon_IsletaDiv <- read.csv("Results/Reconstruction/IsletaDiversionReconstruction.csv")
Recon_IsletaRet <- read.csv("Results/Reconstruction/IsletaReturnReconstruction.csv")
Recon_IsletaTemp <- read.csv("Results/Reconstruction/IsletaTempReconstruction.csv")
Recon_IsletaPrecip <- read.csv("Results/Reconstruction/IsletaPrecipReconstruction.csv")

dat<- as.data.frame(cbind(Recon_IsletaExt$dates, 
                          Recon_IsletaExt$standardized, Recon_IsletaExt$signal,
                          Recon_BosqDisch$standardized, Recon_BosqDisch$signal,
                          Recon_IsletaDiv$standardized, Recon_IsletaDiv$signal,
                          Recon_IsletaRet$standardized, Recon_IsletaRet$signal,
                          Recon_IsletaTemp$standardized, Recon_IsletaTemp$signal,
                          Recon_IsletaPrecip$standardized, Recon_IsletaPrecip$signal))
dat_isleta <- dat %>% 
  rename(Dates = 1,
         Extent_observed = 2, Extent_signal = 3,
         Discharge_observed = 4, Discharge_signal = 5,
         Diversion_observed = 6, Diversion_signal = 7,
         Returns_observed = 8, Returns_signal = 9,
         Temperature_observed = 10, Temperature_signal = 11,
         Precipitation_observed = 12, Precipitation_signal = 13) %>% 
  mutate(Dates = as.Date(Dates, format = "%Y-%m-%d")) 

#Plotting data and signal
df_long <- dat_isleta %>%
  pivot_longer(cols = -Dates,  # Everything except 'Date' should be pivoted
               names_to = c("Variable", "Type"),  # Separate into 'Variable' and 'Type'
               names_sep = "_") %>%
  mutate(Variable = factor(Variable, levels = c("Extent", "Discharge", "Diversion", "Returns", 
                                                "Precipitation", "Temperature"))) %>% 
  mutate(Variable = fct_recode(Variable, "Extent dry" = "Extent"))

jpeg("Figures/IsletaObservedSignal.jpg", width = 800, height =500, res = 100)
ggplot(df_long, aes(x = Dates, y = value, color = Type)) +
  geom_line() +
  facet_wrap(vars(Variable)) +
  scale_color_manual(values = c("observed" = "black", "signal" = "red")) +  # Set colors manually
  labs(title = "Isleta reach", x = "", y = "scaled value", color = "") +
  theme_classic()+
  theme(legend.position = c(0.12, 0.94),
        legend.direction = "horizontal",  # Arrange legend items in a single row
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA))
dev.off()

#Plotting data and components 

Pl_Extent <- Recon_IsletaExt %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_1963, cycle_NA), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta extent dry", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_1963" = "#D55E00", 
                                "cycle_NA" = "#56B4E9"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day ", 
                                "cycle_1963" = "1963-day ", 
                                "cycle_NA" = "unknown"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 


Pl_IsletaDiv <- Recon_IsletaDiv %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_182, cycle_91, cycle_73, cycle_122), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta dam diversion", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_182" = "#D55E00", 
                                "cycle_91" = "#56B4E9",
                                "cycle_73" = "#F0E442",
                                "cycle_122" = "#009E73"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day", 
                                "cycle_182" = "182-day", 
                                "cycle_91" = "91-day",
                                "cycle_73" = "73-day",
                                "cycle_122" = "122-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 

Pl_BosqDisch <- Recon_BosqDisch %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_709, cycle_406, cycle_1407), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Bosque Farms discharge", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_709" = "#0072B2", 
                                "cycle_406" = "#D55E00", 
                                "cycle_1407" = "#56B4E9"),
                     labels = c("standardized" = "ts", 
                                "cycle_709" = "709-day", 
                                "cycle_406" = "406-day", 
                                "cycle_1407" = "1407-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 


Pl_IsletaReturn <- Recon_IsletaRet %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_361, cycle_184, cycle_777, cycle_254), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach returns", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_361" = "#0072B2", 
                                "cycle_184" = "#D55E00", 
                                "cycle_777" = "#56B4E9",
                                "cycle_254" = "#F0E442"),
                     labels = c("standardized" = "ts", 
                                "cycle_361" = "361-day", 
                                "cycle_184" = "184-day", 
                                "cycle_777" = "777-day",
                                "cycle_254" = "254-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 

Pl_IsletaTemp <- Recon_IsletaTemp %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_365, cycle_181, cycle_121, cycle_74), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach temperature", x = "", y = "standardized values") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_365" = "#0072B2", 
                                "cycle_181" = "#D55E00", 
                                "cycle_121" = "#56B4E9",
                                "cycle_74" = "#F0E442"),
                     labels = c("standardized" = "ts", 
                                "cycle_365" = "365-day", 
                                "cycle_181" = "181-day", 
                                "cycle_121" = "121-day",
                                "cycle_74" = "74-day"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3)) 



Pl_IsletaPrecip <- Recon_IsletaPrecip %>% 
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(standardized, cycle_364, cycle_57, cycle_25, cycle_17, cycle_NA), 
               names_to = "category", 
               values_to = "values") %>% 
  ggplot(aes(x = dates, y = values, color = category)) +
  geom_line(data = . %>% filter(category == "standardized"), aes(color = category), size = 0.5) +
  geom_line(data = . %>% filter(category != "standardized"), aes(color = category), size = 1.) +
  theme_classic() +
  labs(title = "Isleta reach precipitation", x = "", y = "") +
  scale_color_manual(values = c("standardized" = "black", 
                                "cycle_364" = "#0072B2", 
                                "cycle_57" = "#D55E00", 
                                "cycle_25" = "#56B4E9",
                                "cycle_17" = "#F0E442",
                                "cycle_NA" = "#CC79A7"),
                     labels = c("standardized" = "ts", 
                                "cycle_364" = "364-day", 
                                "cycle_57" = "57-day", 
                                "cycle_25" = "25-day",
                                "cycle_17" = "17-day",
                                "cycle_NA" = "unknown"),
                     name = "")+
  theme(legend.position = "top", legend.key.height = unit(0.4, "cm"), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank())+
  guides(color = guide_legend(ncol = 3))  

jpeg("Figures/IsletaReconstruction.jpg", width = 800, height =990, res = 100)

plot_grid(Pl_Extent, Pl_BosqDisch, Pl_IsletaDiv, Pl_IsletaReturn, Pl_IsletaTemp, Pl_IsletaPrecip,
          ncol = 2, nrow = 3)

dev.off()
