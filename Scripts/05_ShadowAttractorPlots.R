#libraries ####
library(tidyverse)
library(scatterplot3d)
library(cowplot)
library(magick)

#data ####
ExtentIsl_Mx <- read.csv("Results/Mx/IsletaExtent_Mx.csv")
BosqueDischarge_Mx <- read.csv("Results/Mx/BosqueDischarge_Mx.csv") %>% select(!X)
DiversionIsl_Mx <- read.csv("Results/Mx/IsletaDiversion_Mx.csv") 
ReturnsIsl_Mx <- read.csv("Results/Mx/IsletaReturns_Mx.csv")
PrecipIsl_Mx <- read.csv("Results/Mx/IsletaPrecip_Mx.csv")
TempIsl_Mx <- read.csv("Results/Mx/IsletaTemp_Mx.csv")%>% select(!X)

#Isleta Extent ####
jpeg("Figures/Shadow_ExtentIsleta.jpg", width = 800, height = 600, res = 100)
ShadExtent_PL <- scatterplot3d(ExtentIsl_Mx, type = "l", main="Extent dry",
                     xlab = "t",   # Title for the X axis
                     ylab = "",   # Title for the Y axis
                     zlab = "t+178", 
                     lwd = 2, 
                     col.axis = "black",
                     cex.lab = 2,
                     cex.main = 2)
# Manually add the Y-axis label
text(x = ShadExtent_PL$xyz.convert(2.5, max(ExtentIsl_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadExtent_PL$xyz.convert(0, max(ExtentIsl_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+89", 
     srt = 36,               # Adjust the angle of the text
     adj = 1,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #
dev.off()



#Bosque Discharge ####
jpeg("Figures/Shadow_BosqueDischarge.jpg", width = 800, height = 600, res = 100)
ShadDischarge_PL <- scatterplot3d(BosqueDischarge_Mx, type = "l", main="Discharge",
                               xlab = "t",   # Title for the X axis
                               ylab = "",   # Title for the Y axis
                               zlab = "t+144", 
                               lwd = 2, 
                               col.axis = "black",
                               cex.lab = 2,
                               cex.main = 2)

# Manually add the Y-axis label
text(x = ShadDischarge_PL$xyz.convert(4, max(BosqueDischarge_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadDischarge_PL$xyz.convert(0, max(BosqueDischarge_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+72", 
     srt = 36,               # Adjust the angle of the text
     adj = 3,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()

#Isleta Diversion ####
jpeg("Figures/Shadow_IsletaDiversion.jpg", width = 800, height = 600, res = 100)
ShadDiversion_PL <- scatterplot3d(DiversionIsl_Mx, type = "l", main="Diversion",
                                  xlab = "t",   # Title for the X axis
                                  ylab = "",   # Title for the Y axis
                                  zlab = "t+152", 
                                  lwd = 2, 
                                  col.axis = "black",
                                  cex.lab = 2,
                                  cex.main = 2)

# Manually add the Y-axis label
text(x = ShadDiversion_PL$xyz.convert(2.5, max(DiversionIsl_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadDiversion_PL$xyz.convert(0, max(DiversionIsl_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+76", 
     srt = 36,               # Adjust the angle of the text
     adj =3,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()

#Isleta Returns ####
jpeg("Figures/Shadow_IsletaReturns.jpg", width = 800, height = 600, res = 100)
ShadReturns_PL <- scatterplot3d(ReturnsIsl_Mx, type = "l", main="Returns",
                                  xlab = "t",   # Title for the X axis
                                  ylab = "",   # Title for the Y axis
                                  zlab = "t+174", 
                                  lwd = 2, 
                                  col.axis = "black",
                                cex.lab = 2,
                                cex.main = 2)

# Manually add the Y-axis label
text(x = ShadReturns_PL$xyz.convert(2.5, max(ReturnsIsl_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadReturns_PL$xyz.convert(0, max(ReturnsIsl_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+87", 
     srt = 36,               # Adjust the angle of the text
     adj =2,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()



#Isleta Precipitation ####
jpeg("Figures/Shadow_IsletaPrecip.jpg", width = 800, height = 600, res = 100)
ShadPrecip_PL <- scatterplot3d(PrecipIsl_Mx, type = "l", main="Precipitation",
                                xlab = "t",   # Title for the X axis
                                ylab = "",   # Title for the Y axis
                                zlab = "t+196", 
                                lwd = 2, 
                                col.axis = "black",
                               cex.lab = 2,
                               cex.main = 2)

# Manually add the Y-axis label
text(x = ShadPrecip_PL$xyz.convert(0.2, max(PrecipIsl_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadPrecip_PL$xyz.convert(0, max(PrecipIsl_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+98", 
     srt = 36,               # Adjust the angle of the text
     adj =-2.5,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()
#Isleta Temperature ####
jpeg("Figures/Shadow_IsletaTemp.jpg", width = 800, height = 600, res = 100)
ShadTemp_PL <- scatterplot3d(TempIsl_Mx, type = "l", main="Temperature",
                              xlab = "t",   # Title for the X axis
                              ylab = "",   # Title for the Y axis
                              zlab = "t+124", 
                              lwd = 2, 
                              col.axis = "black",
                              cex.lab = 2,
                             cex.main = 2)

# Manually add the Y-axis label
text(x = ShadTemp_PL$xyz.convert(2.5, max(TempIsl_Mx[,2]), 0)$x,  # X coordinate for the label
     y = ShadTemp_PL$xyz.convert(0, max(TempIsl_Mx[,2]), -1.8)$y,  # Y coordinate for the label
     labels = "t+62", 
     srt = 36,               # Adjust the angle of the text
     adj = 3.5,                # Adjust the text alignment
     xpd = TRUE,             # Allow text to be drawn outside the plot area
     cex = 2)              #

dev.off()
#combine images ####

img1 <- image_read("Figures/Shadow_ExtentIsleta.jpg")
img2 <- image_read("Figures/Shadow_BosqueDischarge.jpg")
img3 <- image_read("Figures/Shadow_IsletaDiversion.jpg")
img4 <- image_read("Figures/Shadow_IsletaReturns.jpg")
img5 <- image_read("Figures/Shadow_IsletaPrecip.jpg")
img6 <- image_read("Figures/Shadow_IsletaTemp.jpg")

jpeg("Figures/CombinedShadowIsleta.jpg", width = 3600, height = 4800, res = 600)

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

