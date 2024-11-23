#READ ME ####
#The purpose of this script is to measure the transfer entropy among
#variables themselves and Isleta Reach drying

#Libraries ####
library(tidyverse)
library(RTransferEntropy)
library(tseries)#testing stationarity
library(fable)
library(future)
library(fpp3)
library(lubridate)

#plan(multisession)

#Data ####
dattemp <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  select(Extent, Precip_mm, Temp_C, Diversion_cfs, Returns_cfs)

BosqueFarms <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(dates, Discharge_cfs)

dat <- cbind(dattemp, BosqueFarms) %>% 
  select(dates, Extent, Discharge_cfs, Diversion_cfs,
         Returns_cfs, Temp_C, Precip_mm)

#Stationarity ####
adf_results <- lapply(dat, adf.test, k=10)

adf_table <- tibble(
  Variable = names(adf_results),
  Dickey_Fuller_Value = sapply(adf_results, function(x) x$statistic),
  Lag_Order = sapply(adf_results, function(x) x$parameter),
  P_Value = sapply(adf_results, function(x) x$p.value),
  Conclusion = sapply(adf_results, function(x) {
    if (x$p.value < 0.05) {
      "Stationary"
    } else {
      "Not stationary"
    }
  })
)

# KPSS test
kpss_results <- lapply(dat_final, kpss.test, null = "Trend")

# Print results for each time series
for (i in 1:length(kpss_results)) {
  cat("KPSS Test Results for Series", names(dat)[i], ":\n")
  print(kpss_results[[i]])
  cat("\n")
}  

kpss_table <- tibble(
  Variable = names(kpss_results),
  KPSS_Level = sapply(kpss_results, function(x) x$statistic),
  Lag_Parameter = sapply(kpss_results, function(x) x$parameter),
  P_Value = sapply(kpss_results, function(x) x$p.value)
)

#Detrend and Retest ####

# Create a tsibble for easier time series manipulation
dat_ts <- dat %>%
  select(!dates) %>% 
  mutate(time = seq(length(Extent))) %>% 
  as_tsibble(index = time) 


# Function to decompose, detrend, and test for stationarity
detrend_and_test <- function(series_name) {
  # Decompose the series
  decomposed <- dat_ts %>% 
    model(STL(as.formula(paste(series_name, "~ trend()")), robust = TRUE)) %>% 
    components()
  
  # Detrend the series
  detrended <- decomposed %>% 
    mutate(detrended = !!sym(series_name) - trend)
  
  # KPSS tests for level and trend stationarity
  kpss_level <- kpss.test(detrended$detrended, null = "Level")
  kpss_trend <- kpss.test(detrended$detrended, null = "Trend")
  
  # Return results as a list
  list(
    Variable = series_name,
    Detrended_Data = detrended$detrended,
    KPSS_Level = kpss_level$statistic,
    P_Value_Level = kpss_level$p.value,
    KPSS_Trend = kpss_trend$statistic,
    P_Value_Trend = kpss_trend$p.value
  )
}

# Apply the function to each variable
variables <- c("Extent", "Discharge_cfs", "Diversion_cfs", "Returns_cfs", "Temp_C", "Precip_mm")
detrended_results <- lapply(variables, detrend_and_test)

# Convert the results to a tidy data frame
kpss_results_table <- do.call(rbind, lapply(detrended_results, function(x) data.frame(
  Variable = x$Variable,
  KPSS_Level = x$KPSS_Level,
  P_Value_Level = x$P_Value_Level,
  KPSS_Trend = x$KPSS_Trend,
  P_Value_Trend = x$P_Value_Trend
))) %>%
  as_tibble()

# Extract detrended series and combine them into a final data frame
final_detrended_data <- bind_cols(
  do.call(cbind, lapply(detrended_results, function(x) x$Detrended_Data))
) %>%
  setNames(paste0(variables, "_detrended"))

detrended_dates<- cbind(dat$dates,final_detrended_data) %>% 
  rename(dates = 1) %>% 
  filter(between(month(dates), 4, 10))

# Print the resulting tables
print(kpss_results_table)

#Transfer Entropy ####
#results don't make sense

test <- transfer_entropy(final_detrended_data$Extent_detrended,
                         final_detrended_data$Temp_C_detrended, nboot = 100, entropy = "Shannon",
                         lx = 7, ly = 7, bins = 10)

test2 <- transfer_entropy(dat$Extent, dat$Diversion_cfs, nboot = 50, entropy = "Shannon")

test3 <- transfer_entropy(detrended_dates$Extent_detrended, detrended_dates$Discharge_cfs_detrended, nboot = 50,
                          entropy = "Shannon", bins = 20)