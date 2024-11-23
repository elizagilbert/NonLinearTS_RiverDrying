#libraries ####
library(tidyverse)
library(scatterplot3d)
library(cowplot)
library(magick)

#data ####
ExtentSanAcacia_Mx <- read.csv("Results/Mx/SanAcaciaExtent_Mx.csv")
EscondidaDischarge_Mx <- read.csv("Results/Mx/EscondidaDischarge_Mx.csv")
DiversionSanAcacia_Mx <- read.csv("Results/Mx/SanAcaciaDiversion_Mx.csv") 
ReturnsSanAcacia_Mx <- read.csv("Results/Mx/SanAcaciaReturns_Mx.csv")
PrecipSanAcacia_Mx <- read.csv("Results/Mx/SanAcaciaPrecip_Mx.csv")
TempSanAcacia_Mx <- read.csv("Results/Mx/SanAcaciaTemp_Mx.csv")

#SanAcacia Extent ####
jpeg("Figures/Shadows/Shadow_ExtentSanAcacia.jpg", width = 800, height = 600, res = 100)
ShadExtent_PL <- scatterplot3d(ExtentSanAcacia_Mx, type = "l", main="Extent dry",
                     xlab = "t",   # Title for the X axis
                     ylab = "",   # Title for the Y axis
                     zlab = "t+198", 
                     lwd = 2, 
                     col.axis = "black",
                     cex.lab = 2,
                     cex.main = 2)
# Manually add the Y-axis label
text(x = ShadExtent_PL$xyz.convert(2.5, max(ExtentSanAcacia_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadExtent_PL$xyz.convert(0, max(ExtentSanAcacia_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+99", 
     srt = 36,               # Adjust the angle of the text
     adj = 0.5,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #
dev.off()



#Escondida Discharge ####
jpeg("Figures/Shadows/Shadow_EscondidaDischarge.jpg", width = 800, height = 600, res = 100)
ShadDischarge_PL <- scatterplot3d(EscondidaDischarge_Mx, type = "l", main="Discharge",
                               xlab = "t",   # Title for the X axis
                               ylab = "",   # Title for the Y axis
                               zlab = "t+150", 
                               lwd = 2, 
                               col.axis = "black",
                               cex.lab = 2,
                               cex.main = 2)

# Manually add the Y-axis label
text(x = ShadDischarge_PL$xyz.convert(4, max(EscondidaDischarge_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadDischarge_PL$xyz.convert(0, max(EscondidaDischarge_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+75", 
     srt = 36,               # Adjust the angle of the text
     adj = 3,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()

#SanAcacia Diversion ####
jpeg("Figures/Shadows/Shadow_SanAcaciaDiversion.jpg", width = 800, height = 600, res = 100)
ShadDiversion_PL <- scatterplot3d(DiversionSanAcacia_Mx, type = "l", main="Diversion",
                                  xlab = "t",   # Title for the X axis
                                  ylab = "",   # Title for the Y axis
                                  zlab = "t+152", 
                                  lwd = 2, 
                                  col.axis = "black",
                                  cex.lab = 2,
                                  cex.main = 2)

# Manually add the Y-axis label
text(x = ShadDiversion_PL$xyz.convert(2, max(DiversionSanAcacia_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadDiversion_PL$xyz.convert(0, max(DiversionSanAcacia_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+76", 
     srt = 36,               # Adjust the angle of the text
     adj =1,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()

#SanAcacia Returns ####
jpeg("Figures/Shadows/Shadow_SanAcaciaReturns.jpg", width = 800, height = 600, res = 100)
ShadReturns_PL <- scatterplot3d(ReturnsSanAcacia_Mx, type = "l", main="Returns",
                                  xlab = "t",   # Title for the X axis
                                  ylab = "",   # Title for the Y axis
                                  zlab = "t+136", 
                                  lwd = 2, 
                                  col.axis = "black",
                                cex.lab = 2,
                                cex.main = 2)

# Manually add the Y-axis label
text(x = ShadReturns_PL$xyz.convert(3, max(ReturnsSanAcacia_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadReturns_PL$xyz.convert(0, max(ReturnsSanAcacia_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+68", 
     srt = 36,               # Adjust the angle of the text
     adj =1,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()



#SanAcacia Precipitation ####
jpeg("Figures/Shadows/Shadow_SanAcaciaPrecip.jpg", width = 800, height = 600, res = 100)
ShadPrecip_PL <- scatterplot3d(PrecipSanAcacia_Mx, type = "l", main="Precipitation",
                                xlab = "t",   # Title for the X axis
                                ylab = "",   # Title for the Y axis
                                zlab = "t+136", 
                                lwd = 2, 
                                col.axis = "black",
                               cex.lab = 2,
                               cex.main = 2)

# Manually add the Y-axis label
text(x = ShadPrecip_PL$xyz.convert(-.2, max(PrecipSanAcacia_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadPrecip_PL$xyz.convert(0, max(PrecipSanAcacia_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+68", 
     srt = 36,               # Adjust the angle of the text
     adj =-5,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()
#SanAcacia Temperature ####
jpeg("Figures/Shadows/Shadow_SanAcaciaTemp.jpg", width = 800, height = 600, res = 100)
ShadTemp_PL <- scatterplot3d(TempSanAcacia_Mx, type = "l", main="Temperature",
                              xlab = "t",   # Title for the X axis
                              ylab = "",   # Title for the Y axis
                              zlab = "t+196", 
                              lwd = 2, 
                              col.axis = "black",
                              cex.lab = 2,
                             cex.main = 2)

# Manually add the Y-axis label
text(x = ShadTemp_PL$xyz.convert(2.5, max(TempSanAcacia_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadTemp_PL$xyz.convert(0, max(TempSanAcacia_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+98", 
     srt = 36,               # Adjust the angle of the text
     adj = 2.5,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()
#combine images ####

img1 <- image_read("Figures/Shadows/Shadow_ExtentSanAcacia.jpg")
img2 <- image_read("Figures/Shadows/Shadow_EscondidaDischarge.jpg")
img3 <- image_read("Figures/Shadows/Shadow_SanAcaciaDiversion.jpg")
img4 <- image_read("Figures/Shadows/Shadow_SanAcaciaReturns.jpg")
img5 <- image_read("Figures/Shadows/Shadow_SanAcaciaPrecip.jpg")
img6 <- image_read("Figures/Shadows/Shadow_SanAcaciaTemp.jpg")

jpeg("Figures/CombinedShadowSanAcacia.jpg", width = 3600, height = 4800, res = 600)

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

