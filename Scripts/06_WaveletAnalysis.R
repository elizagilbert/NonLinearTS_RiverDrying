library(WaveletComp)
library(tidyverse)

#this is from the waveletcomp_guide ####
  #series with constant period
x = periodic.series(start.period = 50, length = 1000)
x = x + 0.2*rnorm(1000) # add some noise
my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2),
            legend.coords = "bottomleft", ylim = c(-1.8, 1.8))

  #series with variable period and linearly increasing trend
x = periodic.series(start.period = 20, end.period = 100, length = 1000)
x = x + 0.2*rnorm(1000)

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"))

my.rec <- reconstruct(my.w) #get reconstructed series, will have not trend produced unless loess.span = 1
x.rec <- my.rec$series$x.r # x: name of original series

  #a series with two periods
x1 <- periodic.series(start.period = 80, length = 1000)
x2 <- periodic.series(start.period = 30, length = 1000)
x <- x1 + x2 + 0.2*rnorm(1000)

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels") )

reconstruct(my.w, sel.period = 80, plot.waves = TRUE, lwd = c(1,2),
            legend.coords = "bottomleft") #only uses period 80 could also use sel.period = c(30, 80),

   #average power comparing two times series
x1 <- periodic.series(start.period = 100, length = 500)
x2 <- 1.2*periodic.series(start.period =60, length = 500)
x <- c(x1, x2) + 0.3*rnorm(1000)

y1 <- periodic.series(start.period = 100, length = 1000)
y2 <- 1.2*periodic.series(start.period = 60, length = 1000)
y <- (y1 + y2)/2 + 0.3*rnorm(1000)

my.data <- data.frame(x = x, y = y)
my.wx <- analyze.wavelet(my.data, "x", loess.span = 0,
                         dt = 1, dj = 1/20, #used low resolution to obtain isolated dots as significance indicators
                         lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)
my.wy <- analyze.wavelet(my.data, "y", loess.span = 0,
                         dt = 1, dj = 1/20,
                         lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)

#comparison of average powers
maximum.level = 1.001*max(my.wx$Power.avg, my.wy$Power.avg)
wt.avg(my.wx, maximum.level = maximum.level)
wt.avg(my.wy, maximum.level = maximum.level)

#selecting the method of analysis
x1 <- periodic.series(start.period = 100, length = 400)
x2 <- 1.2*periodic.series(start.period = 50, length = 200)
x <- c(x1, x2, x1) + 0.2*rnorm(1000)

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        method = "white.noise",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 32, upperPeriod = 256,
                        make.pval = TRUE, n.sim = 10) 
wt.image(my.w, color.key = "interval", n.levels = 250, 
         legend.params = list(lab = "wavelet power levels"))#can use siglvl = 0.05

#Because Fourier randomization assumes that there is constant periodicity over time, 
#it only detects the deviations from the average periodicity in the middle.
my.w <- analyze.wavelet(my.data, "x",
                        method = "Fourier.rand",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 32, upperPeriod = 256,
                        make.pval = TRUE, n.sim = 10) 
wt.image(my.w, color.key = "interval", n.levels = 250, 
         legend.params = list(lab = "wavelet power levels"))

  #plotting the power spectrum - plotting functions
wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", label.digits = 2)) #color.key quantile

#time axis styles

epoch.seq <- seq(from = as.POSIXct("2018-01-01 00:00:00"),
                 to = as.POSIXct("2018-01-30 23:00:00"), by = 3600)
x <- periodic.series(start.period = 24, length = 720)
x <- x + rnorm(720)
my.data <- data.frame(date = epoch.seq, x = x)

my.w <- analyze.wavelet(my.data, "x", loess.span = 0, dt = 1/24, dj = 1/500,
                        lowerPeriod = 1/4, upperPeriod = 2, make.pval = FALSE)


wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab = "wavelet power levels"),
         label.time.axis = TRUE, # default setting
         spec.time.axis = list(at = seq(1, 720, by = 48),
                               labels = seq(0, 28, by = 2)))

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab = "wavelet power levels"),
         label.time.axis = TRUE, # default setting
         spec.time.axis = list(at = seq(1, 720, by = 50),
                      labels = seq(0, 719, by = 50)))

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab = "wavelet power levels"),
         label.time.axis = TRUE, # default setting
         show.date = TRUE, date.format = "%F %T")

ticks <- seq(as.POSIXct("2018-01-01 00:00:00", format = "%F %T"),
             as.POSIXct("2018-01-31 23:00:00", format = "%F %T"), by = "week")
labels <- seq(as.Date("2018-01-01"), as.Date("2018-01-29"), by = "week")
labels <- paste("Mon, ", labels)
wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab = "wavelet power levels"),
         label.time.axis = TRUE, # default setting
         show.date = TRUE, date.format = "%F %T",
         spec.time.axis = list(at = ticks, labels = labels, las = 2))

  ### bivariate time series
#ex: constant periods

x1 <- periodic.series(start.period = 1*24, length = 24*96)
x2 <- periodic.series(start.period = 2*24, length = 24*96)
x3 <- periodic.series(start.period = 4*24, length = 24*96)
x4 <- periodic.series(start.period = 8*24, length = 24*96)
x5 <- periodic.series(start.period = 16*24, length = 24*96)
x <- x1 + x2 + 3*x3 + x4 + x5 + 0.5*rnorm(24*96)
y <- x1 + x2 - 3*x3 + x4 + 3*x5 + 0.5*rnorm(24*96)

my.data <- data.frame(x = x, y = y)
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1/24, dj = 1/100,
                           lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (days)")
wc.avg(my.wc, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (days)")

#variable periods, power spectrum

xx <- periodic.series(start.period = 64, length = 128*3)
xy <- periodic.series(start.period = 128, length = 2*128*3)
x <- c(xx,xy,xx) + 0.2*rnorm(4*128*3)
y <- periodic.series(start.period = 128, phase = -16, length = 4*128*3) +
  0.2*rnorm(4*128*3)

my.data <- data.frame(x = x, y = y)
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1, dj = 1/100,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05, ## default values
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "")

# limits region to where both wavelet transformations are significant
wc.image(my.wc, n.levels = 250, color.key = "interval",
         siglvl.contour = 0.1, siglvl.arrow = 0.05, which.arrow.sig = "wt",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "")

#variable period coherence
wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "wavelet coherence levels"),
         timelab = "") #using which.image is wavelet coherence instead of default wavelet power
 
  #different smoothing
my.wc2 <- analyze.coherency(my.data, my.pair = c("x","y"),
                            loess.span = 0, dt = 1, dj = 1/100,
                            window.type.t = 1, window.type.s = 1,
                            window.size.t = 5, window.size.s = 1,
                            make.pval = TRUE, n.sim = 10)
wc.image(my.wc2, which.arrow.sig = "wt")

my.wc3 <- analyze.coherency(my.data, my.pair = c("x","y"),
                            loess.span = 0, dt = 1, dj = 1/100,
                            window.type.t = 3, window.type.s = 3,
                            window.size.t = 5, window.size.s = 1,
                            make.pval = TRUE, n.sim = 10)


wc.image(my.wc3, which.arrow.sig = "wt")

#variable periods and phase differences
#using data and wc from variable periods power spectrums

wc.sel.phases(my.wc, sel.period = 128,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.05,
              phaselim = c(-pi,+pi), ## default if legend.horiz = FALSE
              legend.coords = "topright", legend.horiz = FALSE,
              main = "", sub = "", timelab = "")

at.ticks <- seq(from = -pi, to = pi, by = pi/4)
label.ticks <- (at.ticks/pi)*(128/2)
wc.sel.phases(my.wc, sel.period = 128,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.05,
              phaselim = c(-pi,+pi),
              phaselab = "phases (time units)",
              spec.phase.axis = list(at = at.ticks, labels = label.ticks),
              legend.coords = "topright", legend.horiz = FALSE,
              main = "", sub = "", timelab = "")

wc.phasediff.image(my.wc, which.contour = "wc", use.sAngle = TRUE,
                   n.levels = 250, siglvl = 0.1,
                   legend.params = list(lab = "phase difference levels",
                                        lab.line = 3),
                   timelab = "")


#real life examples ####

#number of trades in a 5 minute window
#asking to understand periodic behavior and estimate intensity of transactions through an active cycle
data(FXtrade.transactions)

my.w <- analyze.wavelet(my.data = FXtrade.transactions, "transactions",
                        dt = 1/(12*24), lowerPeriod = 1/8.)

wt.image(my.w) #1day period interrupted by weekends

my.data.a <- FXtrade.transactions[FXtrade.transactions$active == TRUE, ]
my.w.a <- analyze.wavelet(my.data.a, "transactions",
                          loess.span = 0.0, # no detrending required
                          dt = 1/(12*24), # one day has 12*24 5-minute time slots
                          dj = 1/50, # resolution along period axis
                          lowerPeriod = 1/8, # lowest period of interest: 3 hours
                          make.pval = TRUE, # draws white lines indicating significance
                          n.sim = 10) # higher number will give smoother white lines

at <- seq(1, nrow(my.data.a), by = 12*24) # every active day at 00:00:00
labels <- strftime(as.POSIXct(my.data.a$date[at],
                              format="%F %T", tz = "GMT"), format ="%b %d")
wt.image(my.w.a, n.levels = 250, periodlab = "period (active days)",
         legend.params = list(lab = "wavelet power levels"),
         spec.time.axis = list(at = at, labels = labels))

my.rec.a <- reconstruct(my.w.a, plot.waves = FALSE,
                        spec.time.axis = list(at = at, labels = labels))
transactions.rec.a <- my.rec.a$series$transactions.r
transactions.rec.a[transactions.rec.a < 0] <- 0 # some values are negative

transactions.rec <- rep(0, nrow(FXtrade.transactions))
transactions.rec[FXtrade.transactions$active == TRUE] <- transactions.rec.a
FXtrade.transactions <- data.frame(FXtrade.transactions,
                                   transactions.rec = transactions.rec)

transactions.cycle <- matrix(transactions.rec.a[-(1:540)], ncol = 4)
transactions.cycle <- rowMeans(transactions.cycle)/5 # transactions per minute
transactions.cycle <- data.frame(avg = transactions.cycle,
                                 time = substr(my.data.a$date[541:1980],12,16))

plot(transactions.cycle$avg, type = "l", xaxt = "n", yaxt = "n",
     xlab = "", ylab = "weekly transaction intensity",
     ylim = c(0, max(transactions.cycle$avg)))
index.sel <- seq(37, length(transactions.cycle$avg), by = 72)
axis(1, labels = as.character(transactions.cycle$time[index.sel]),
     at = index.sel)
axis(2, labels = seq(0, 200, by = 50),
     at = seq(0, 200, by = 50))
mtext(text = c("Mon", "Tue", "Wed", "Thu", "Fri"), side = 1, line = 2,
      at = seq(181, length(transactions.cycle$avg), by = 4*72))


#marriages in Turkey
data(marriages.Turkey)
m.adj <- marriages.Turkey$marriages*(4/marriages.Turkey$n.Sun)
my.data <- data.frame(marriages.Turkey, m.adj = m.adj, log.m.adj = log(m.adj))

#goal to understand structure of seasonality
#compute and plot wavelet power

my.w <- analyze.wavelet(my.data, "log.m.adj",
                        loess.span = 3/26,
                        dt = 1, dj = 1/250,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (months)", show.date = TRUE, timelab = "")

#better understand period 8 to 16

my.w <- analyze.wavelet(my.data, "log.m.adj",
                        lowerPeriod = 8, upperPeriod = 16,
                        loess.span = 3/26,
                        dt = 1, dj = 1/1000,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (months)", show.date = TRUE, timelab = "",
         spec.period.axis = list(at = c(8, 10, 12, 14, 16)),
         graphics.reset = FALSE)
abline(h = log(12)/log(2))
mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)

#does islamic calendar influence marrigge seasonality

ridge <- my.w$Period * my.w$Ridge
ridge <- colSums(ridge)
ridge[ridge == 0] <- NA

#weather and radiation
data(weather.radiation.Mannheim)

my.w <- analyze.wavelet(weather.radiation.Mannheim, "radiation",
                        loess.span = 0,
                        dt = 1, dj = 1/50,
                        lowerPeriod = 32, upperPeriod = 1024,
                        make.pval = TRUE, n.sim = 10)

wt.image(my.w, color.key = "interval", n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (days)",
         show.date = TRUE, date.format = "%F", timelab = "")


#need to have all power levels have the same maximum
max.power <- max(my.w$Power)

wt.image(my.w, color.key = "interval", n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (days)",
         maximum.level = 1.001 * max.power,
         show.date = TRUE, date.format = "%F", timelab = "")


wt.image(my.w, color.key = "interval", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", label.digits = 2),
         periodlab = "periods (days)", maximum.level = 1.25,
         # Concerning item 1 above --- plot the square root of power:
         exponent = 0.5,
         # Concerning item 2 above --- time axis:
         show.date = TRUE, date.format = "%F", timelab = "",
         spec.time.axis = list(at = c(paste(2005:2014, "-01-01", sep = "")),
                               labels = c(2005:2014)),
         timetcl = -0.5, # draws outward ticks
         # Concerning item 3 above --- period axis:
         spec.period.axis =
           list(at = c(32, 64, 128, 365, 1024)),
         periodtck = 1, periodtcl = NULL # draws horizontal lines
)

#cross-wavelet transformation which series is leading at a given time and period

my.wc <- analyze.coherency(weather.radiation.Mannheim,
                           my.pair = c("temperature", "humidity"),
                           loess.span = 0,
                           dt = 1, dj = 1/50,
                           lowerPeriod = 32, upperPeriod = 1024,
                           make.pval = TRUE, n.sim = 10)
max.power <- max(my.wc$Power.xy) # for plotting

exponent <- 0.5
wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         color.key = "interval",
         maximum.level = (1.001*max.power)^exponent, exponent = exponent,
         # time axis:
         label.time.axis = TRUE, show.date = TRUE,
         spec.time.axis = list(at = paste(2005:2014, "-01-01", sep = ""),
                               labels = 2005:2014),
         timetcl = -0.5, # outward ticks
         # period axis:
         periodlab = "period (days)",
         spec.period.axis = list(at = c(32, 64, 128, 365, 1024)),
         periodtck = 1, periodtcl = NULL)

#Trump v Clinton
data(USelection2016.Instagram)

my.data <- apply(USelection2016.Instagram[, 2:5], FUN = "diff", MAR = 2)
my.data <- data.frame(date = USelection2016.Instagram$date[-1], my.data)

#was clinton ahead or was trump ahead or were they in sync

my.wc <- analyze.coherency(my.data, my.pair = c("trump.pos", "clinton.pos"),
                           dt = 1, dj = 1/100,
                           lowerPeriod = 6, upperPeriod = 36,
                           make.pval = TRUE, n.sim = 10)

#to find out which periods important use average cross-wavelet powers
wc.avg(my.wc, exponent = 0.5,
       periodlab = "period (hours)",
       minimum.level = 0, maximum.level = 1.25,
       spec.avg.axis = list(at = seq(0, 1.25, by = 0.25)),
       spec.period.axis = list(at = c(12, 24)),
       periodtck = 1, periodtcl = NULL)













#stop guide
#Isleta #####
dat <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  select(Extent) %>% 
  rename(x = Extent)

dat2 <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Discharge_cfs) %>% 
  rename(y=1)

my.data <- data.frame(x=dat, y=dat2)

 #general wavelet analysis
my.w2 <- analyze.wavelet(dat, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/300,
                        #lowerPeriod = 1, allows default values assigned
                        #upperPeriod = 365,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w2, color.key = "quantile", n.levels = 370,
         legend.params = list(lab = "wavelet power levels", mar = 4.7,
         label.digits = 4))

reconstruct(my.w2, plot.waves = F, lwd = c(1,2), #plot.waves = T or F
            legend.coords = "bottomleft", ylim = c(-10, 30))

#the reconstructed time series
my.rec <- reconstruct(my.w2)

x.rec <- my.rec$series$x.r
rec.val <- my.rec$series$x

dates <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  select(Date) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

recon <- data.frame(Date = dates$Date, reconstructed = x.rec, original = rec.val) %>% 
  pivot_longer(cols = -Date, names_to = "RG", values_to = "Values")

ggplot(recon, aes(x = Date, y = Values, group = RG, color = RG)) +
  geom_line() +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Comparison of x.rec and rec.val",
       x = "Index",
       y = "Value",
       color = "Variable") +
  theme_minimal()

  #compare two series average power
my.wx <- analyze.wavelet(my.data, "x", loess.span = 0,
                         dt = 1, dj = 1/20, #used low resolution to obtain isolated dots as significance indicators
                         #lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)
my.wy <- analyze.wavelet(my.data, "y", loess.span = 0,
                         dt = 1, dj = 1/20,
                         #lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)

#comparison of average powers
maximum.level = 1.001*max(my.wx$Power.avg, my.wy$Power.avg)
wt.avg(my.wx, maximum.level = maximum.level) #extent
wt.avg(my.wy, maximum.level = maximum.level) #discharge at Bosque

  #analysis of bivariate time series
my.wc.drying <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1, dj = 1/20,
                           #lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc.drying, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (days)")

# limits region to where both wavelet transformations are significant
wc.image(my.wc.drying, n.levels = 250, color.key = "interval",
         siglvl.contour = 0.1, siglvl.arrow = 0.05, which.arrow.sig = "wt",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "")

wc.avg(my.wc.drying, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (days)")

#variable periods and phase differences
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1, dj = 1/20,
                           #lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)

wc.sel.phases(my.wc, sel.period = 1000,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.05,
              phaselim = c(-pi,+pi), ## default if legend.horiz = FALSE
              legend.coords = "topright", legend.horiz = FALSE,
              main = "", sub = "", timelab = "")

at.ticks <- seq(from = -pi, to = pi, by = pi/4)
label.ticks <- (at.ticks/pi)*(128/2)
wc.sel.phases(my.wc, sel.period = 64,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.05,
              phaselim = c(-pi,+pi),
              phaselab = "phases (time units)",
              spec.phase.axis = list(at = at.ticks, labels = label.ticks),
              legend.coords = "topright", legend.horiz = FALSE,
              main = "", sub = "", timelab = "")

wc.phasediff.image(my.wc, which.contour = "wc", use.sAngle = TRUE,
                   n.levels = 250, siglvl = 0.1,
                   legend.params = list(lab = "phase difference levels",
                                        lab.line = 3),
                   timelab = "")

