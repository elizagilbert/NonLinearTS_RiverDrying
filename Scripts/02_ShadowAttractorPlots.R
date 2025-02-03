#libraries ####
library(tidyverse)
library(scatterplot3d)
library(cowplot)
library(magick)

#data ####
ExtentIsl_Mx <- read.csv("Results/Mx/TWIsleta_Extent_m2_Mx.csv")
DischargeIsl_Mx <- read.csv("Results/Mx/TWIsleta_Discharge_Mx.csv") 
DiversionIsl_Mx <- read.csv("Results/Mx/TWIsleta_Diversion_Mx.csv") 
ReturnsIsl_Mx <- read.csv("Results/Mx/TWIsleta_Returns_Mx.csv")
PrecipIsl_Mx <- read.csv("Results/Mx/TWIsleta_Precip_Mx.csv")
TempIsl_Mx <- read.csv("Results/Mx/TWIsleta_Temp_m2_Mx.csv")

ExtentSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_Extent_m2_Mx.csv")
DischargeSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_discharge_Mx.csv")
DiversionSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_Diversions_Mx.csv") 
ReturnsSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_Returns_Mx.csv")
PrecipSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_Precip_Mx.csv")
TempSanAcacia_Mx <- read.csv("Results/Mx/TWSanAcacia_Temp_Mx.csv")

# Extent ####
jpeg("Figures/Shadow_Extent.jpg", width = 800, height = 600, res = 100)
ShadExtent_PL <- scatterplot3d(
  x = ExtentIsl_Mx[,1],
  y = ExtentIsl_Mx[,2],
  #z = ExtentIsl_Mx[,3],
  type = "l",
  main = "Drying",
  xlab = "",   # Title for the X axis
  ylab = "",    # Title for the Y axis
  zlab = "",
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2,
  angle = -45,
  phi = 15
)
# Add the new data as another line
ShadExtent_PL$points3d(
  x = ExtentSanAcacia_Mx[,1],
  y = ExtentSanAcacia_Mx[,2],
  #z = ExtentSanAcacia_Mx[,3],
  type = "l",
  col = rgb(1, 0.5, 0, 0.7), # Red with 50% transparency
  lwd = 2
)

dev.off()



# Discharge ####
jpeg("Figures/Shadow_Discharge.jpg", width = 800, height = 600, res = 100)
ShadDischarge_PL <- scatterplot3d(
  x = DischargeIsl_Mx[,1],
  y = DischargeIsl_Mx[,2],
  z = DischargeIsl_Mx[,3],
  type = "l",
  main = "Discharge",
  xlab = "",   # Title for the X axis
  ylab = "",    # Title for the Y axis
  zlab = "",
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2
)

# Add the new data as another line
ShadDischarge_PL$points3d(
  x = DischargeSanAcacia_Mx[,1],
  y = DischargeSanAcacia_Mx[,2],
  z = DischargeSanAcacia_Mx[,3],
  type = "l",
  col = rgb(1, 0.5, 0, 0.8), # Red with 50% transparency
  lwd = 2
)
dev.off()

# Diversion ####
jpeg("Figures/Shadow_Diversion.jpg", width = 800, height = 600, res = 100)
x_range <- range(c(DiversionIsl_Mx[,1], DiversionSanAcacia_Mx[,1]))
y_range <- range(c(DiversionIsl_Mx[,2], DiversionSanAcacia_Mx[,2]))
z_range <- range(c(DiversionIsl_Mx[,3], DiversionSanAcacia_Mx[,3]))

ShadDiversion_PL <- scatterplot3d(
  x = DiversionIsl_Mx[,1],
  y = DiversionIsl_Mx[,2],
  z = DiversionIsl_Mx[,3],
  type = "l",
  main = "Diversions",
  xlab = "",   # Title for the X axis
  ylab = "",   # Title for the Y axis
  zlab = "",
  xlim = x_range,  # Adjust X-axis limits
  ylim = y_range,  # Adjust Y-axis limits
  zlim = z_range,  # Adjust Z-axis limits
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2
)

# Add the new data as another line
ShadDiversion_PL$points3d(
  x = DiversionSanAcacia_Mx[,1],
  y = DiversionSanAcacia_Mx[,2],
  z = DiversionSanAcacia_Mx[,3],
  type = "l",
  col = rgb(1, 0.5, 0, 0.8), # Red with 50% transparency
  lwd = 2
)
dev.off()

# Returns ####
jpeg("Figures/Shadow_Returns.jpg", width = 800, height = 600, res = 100)
ShadReturns_PL <- scatterplot3d(
  x = ReturnsIsl_Mx[,1],
  y = ReturnsIsl_Mx[,2],
  z = ReturnsIsl_Mx[,3],
  type = "l",
  main = "Returns",
  xlab = "",   # Title for the X axis
  ylab = "",    # Title for the Y axis
  zlab = "",
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2
)

# Add the new data as another line
ShadReturns_PL$points3d(
  x = ReturnsSanAcacia_Mx[,1],
  y = ReturnsSanAcacia_Mx[,2],
  z = ReturnsSanAcacia_Mx[,3],
  type = "l",
  col = rgb(1, 0.5, 0, 0.8), # Red with 50% transparency
  lwd = 2
)              #

dev.off()



# Precipitation ####
jpeg("Figures/Shadow_Precip.jpg", width = 800, height = 600, res = 100)
ShadPrcip_PL <- scatterplot3d(
  x = PrecipIsl_Mx[,1],
  y = PrecipIsl_Mx[,2],
  z = PrecipIsl_Mx[,3],
  type = "l",
  main = "Precipitation",
  xlab = "",   # Title for the X axis
  ylab = "",    # Title for the Y axis
  zlab = "",
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2
)

# Add the new data as another line
ShadPrcip_PL$points3d(
  x = PrecipSanAcacia_Mx[,1],
  y = PrecipSanAcacia_Mx[,2],
  z = PrecipSanAcacia_Mx[,2],
  type = "l",
  col = rgb(1, 0.5, 0, 0.8), # Red with 50% transparency
  lwd = 2
)        #

dev.off()

# Temperature ####
jpeg("Figures/Shadow_Temp.jpg", width = 800, height = 600, res = 100)
ShadTemp_PL <- scatterplot3d(
  x = TempIsl_Mx[,1],
  y = TempIsl_Mx[,2],
  #z = TempIsl_Mx[,3],
  type = "l",
  main = "Temperature",
  xlab = "",   # Title for the X axis
  ylab = "",    # Title for the Y axis
  zlab = "",
  lwd = 2,
  col.axis = "black",
  cex.lab = 2,
  cex.main = 2,
  angle = -45,
  phi = 15
)

# Add the new data as another line
ShadTemp_PL$points3d(
  x = TempSanAcacia_Mx[,1],
  y = TempSanAcacia_Mx[,2],
  #z = TempSanAcacia_Mx[,3],
  type = "l",
  col = rgb(1, 0.5, 0, 0.8), # Red with 50% transparency
  lwd = 2
)      #

dev.off()
#combine images ####

img1 <- image_read("Figures/Shadow_Extent.jpg")
img2 <- image_read("Figures/Shadow_Discharge.jpg")
img3 <- image_read("Figures/Shadow_Diversion.jpg")
img4 <- image_read("Figures/Shadow_Returns.jpg")
img5 <- image_read("Figures/Shadow_Precip.jpg")
img6 <- image_read("Figures/Shadow_Temp.jpg")

jpeg("Figures/CombinedShadow.jpg", width = 3600, height = 4800, res = 600)

cowplot::plot_grid(
  cowplot::ggdraw() + cowplot::draw_image(img1) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  cowplot::ggdraw() + cowplot::draw_image(img2) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  cowplot::ggdraw() + cowplot::draw_image(img3) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  cowplot::ggdraw() + cowplot::draw_image(img4) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  cowplot::ggdraw() + cowplot::draw_image(img5) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  cowplot::ggdraw() + cowplot::draw_image(img6) + theme_void() + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0.5), "lines")),
  ncol = 2,
  rel_heights = c(1, 1, 1),  # Ensure equal height for each row
  rel_widths = c(1.05, 1)    # Slightly increase width of the left column to make space
)
dev.off()

