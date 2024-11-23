# Load necessary package
install.packages("vars")
install.packages("tseries")
install.packages("astsa")

library(vars)
library(tseries)
library(tidyverse)
library(astsa)

#generic testing####
#ts1 causes ts2 but ts3 does not cause ts3, so says chat
#used some of my data too and it says discharge does not cause drying and a p(lag)  of 51 days
# Time series data
dates <- seq(as.Date("2000-01-01"), by="month", length.out=100)
# Define the dates and data points
dates <- seq(as.Date("2000-01-01"), by="month", length.out=100)

ts1 <- c(0.496714, -0.138264, 0.647689, 1.523030, -0.234153, -0.234137, 1.579213, 0.767435, -0.469474, 0.542560, 
         -0.463418, -0.465730, 0.241962, -1.913280, -1.724918, -0.562288, -1.012831, 0.314247, -0.908024, -1.412304,
         1.465649, -0.225776, 0.067528, -1.424748, -0.544383, 0.110923, -1.150994, 0.375698, -0.600639, -0.291694,
         -0.601707, 1.852278, -0.013497, -1.057711, 0.822545, -1.220844, 0.208864, -1.959670, -1.328186, 0.196861,
         0.738467, 0.171368, -0.115648, -0.301104, -1.478522, -0.719844, -0.460639, 1.057122, 0.343618, -1.763040,
         0.324084, -0.385082, -0.676922, 0.611676, 1.030999, 0.931280, -0.839218, 0.331263, 1.246438, 1.007189,
         -0.919262, -1.549106, 0.022185, 0.758363, -0.660524, 0.862580, -0.010032, 0.050009, 0.670216, 0.852965,
         -0.955869, -0.023493, -0.652469, -1.218302, 1.074623, 0.723642, -0.503087, -0.622274, -0.921169, -0.726213,
         0.222896, 0.051316, 1.158963, 0.257835, 0.817346, 0.750445, -0.455532, 0.222400, 0.350343, -0.498190,
         -0.785155, 0.015686, 0.895487, 1.178780, -0.179925, -1.070753, 1.054452, -0.403177, 1.222445, 0.208275)

ts2 <- c(-0.200987, 0.355216, 0.860233, 0.855680, -0.522996, -1.070748, 1.089791, 0.231670, 0.070904, 0.579805,
         -0.951892, -0.077012, -0.077257, -0.328115, -0.977829, -0.439583, -0.338548, 0.698843, -1.010457, -1.146946,
         0.517809, -0.262323, -0.489653, -1.247291, -0.479353, 0.070315, -1.304963, 0.366683, -1.140827, 0.235475,
         -0.338744, 1.030059, -0.129649, -1.424261, 0.326745, -0.442724, 0.154958, -1.270581, -0.540848, 0.072603,
         0.230557, 0.125833, -0.666343, -0.166155, -1.418126, -1.114734, -0.383961, 0.821572, 0.073728, -1.553405,
         0.256679, -0.162867, -0.555939, 0.311050, 0.428364, 0.440215, -0.743240, 0.231404, 0.742404, 1.241774,
         -0.751387, -1.097578, 0.102866, 0.558051, -0.443053, 0.670117, -0.052734, 0.168254, 0.823722, 0.489255,
         -0.899400, -0.135017, -0.763564, -1.065145, 0.575069, 0.365037, -0.402417, -0.565084, -0.801631, -0.774084,
         0.045947, -0.046159, 1.137678, 0.089932, 0.495850, 0.564043, -0.485309, 0.035481, 0.245550, -0.392866,
         -0.609153, 0.106686, 0.765747, 1.282432, -0.307309, -0.937313, 1.061208, -0.420580, 1.117283, 0.042856)

ts3 <- c(0.357787, 0.560785, 1.083051, 1.053802, -1.377669, 0.133701, 1.532779, 1.469359, 0.154947, 0.378163,
         -0.887786, -1.980796, -0.347912, 0.156349, 1.230291, 1.202380, -0.387327, -0.302303, -1.048553, -1.420018,
         -1.706270, 1.950775, -0.509652, -0.438074, -1.252795, 0.777490, -1.613898, -0.212740, -0.895467, 0.386902,
         -0.510805, -1.180632, 0.838176, -0.743271, -0.798063, -0.820069, 0.199524, -0.479174, -0.185659, -1.106335,
         -1.196207, 0.812526, 1.356240, -0.072010, 0.503087, 0.231097, -1.099401, -0.172428, -0.877858, 0.042214,
         -0.683728, -0.122890, -0.935769, -0.267888, 0.530355, -0.691661, -0.396754, -0.687173, -1.214078, -1.440796,
         -0.420844, -0.150563, 0.879422, 0.283110, 0.885142, -0.754398, 1.252868, 0.512930, 1.131629, 0.029141,
         0.128983, -2.066954, 0.950088, -0.151357, -0.103219, 0.410599, 0.144044, 1.454274, 0.761038, 1.121674,
         0.443863, 0.333674, 1.494079, -0.205158, 0.313068, 0.653619, 0.864436, -1.017702, 0.038631, 0.492787,
         0.275500, -1.087401, -0.673690, 0.113648, -0.192361, 0.887786, -0.977278, 0.825344, -0.156349, -1.165150)


# Convert to a time series object
data <- data.frame(ts1, ts2, ts3)
data_ts <- ts(data, start=c(2000,1), frequency=12)

dat <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  select(Date, Extent) 

BosqueFarms <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010)

my.data.drying <- data.frame(Date = dat$Date, Extent=dat$Extent, Discharge=BosqueFarms$Discharge_cfs)
data_ts <- as.data.frame(ts(my.data.drying, start = c(2010, 1), frequency = 12))


# Test for stationarity (Augmented Dickey-Fuller Test)
adf.test(data_ts$Extent)
adf.test(data_ts$Discharge)

# If the data is not stationary, difference the data
data_diff <- diff(data_ts)

# Fit a VAR model
# Select the appropriate lag with AIC
dat <- data_ts %>% select(!Date)

lag_selection <- VARselect(dat, lag.max=100, type="both")
p <- lag_selection$selection["AIC(n)"]

# Fit the VAR model
var_model <- VAR(dat, p=p, type="both")

# Check for Granger causality
causality(var_model, cause="Extent")  # Check if Extent Granger-causes Discharge
causality(var_model, cause="Discharge")  # Check if Discharge Granger-causes Extent

# Summary of the VAR model
summary(var_model)

#multivariate testing ####
data <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  select(Extent, Precip_mm, Temp_C, Diversion_cfs, Returns_cfs)
data2 <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R2") %>% 
  select(Extent, Precip_mm, Temp_C, Diversion_cfs, Returns_cfs)
  
BosqueFarms <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Discharge_cfs)
Escondida <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "Escondida") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  mutate(Discharge_cfs = na.approx(Discharge_cfs, na.rm = F))%>% 
  select(Discharge_cfs)

data1 <- cbind(data, BosqueFarms)
data2 <- cbind(data2, Escondida)

adf.test(data1$Extent) #tests for stationary and suggests a lag order 16

data_ts <- ts(data1, start = c(2010, 1), frequency = 12)
plot(data_ts)

data_ts2 <- ts(data2, start = c(2010, 1), frequency = 12)
plot(data_ts2)

#fit VAR
VAR_dat <- VAR(data_ts, p=16, type="both") #fits all possible combinations of response variable, worse with p=2
summary(VAR_dat)

VAR_dat2 <- VAR(data_ts2, p=16, type="both") #fits all possible combinations of response variable
summary(VAR_dat2)

acf(residuals(VAR_dat)[,1]) #bad acf for Extent prediction model lag significant at most days
acf(residuals(VAR_dat2)[,1]) #not horrible for Extent prediction model lag significant at 6 and 22 days only

acf(residuals(VAR_dat)) #diagonal is each model's residuals; cross-correlation are other plots
   #these should not have any significant lags even at 0. Any remaining cross-correlation means model
   #does not accurately capture complete association between these variables in time.
acf(residuals(VAR_dat2))

residuals <- residuals(VAR_dat) #crappy
qqnorm(residuals[,1], main="Q-Q Plot of Residuals")
qqline(residuals[,1], col = "red") 

ggplot(data = as.data.frame(residuals), aes(sample = Extent)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Residuals")

residuals <- residuals(VAR_dat2) #crappy
qqnorm(residuals[,1], main="Q-Q Plot of Residuals")
qqline(residuals[,1], col = "red")
