
# You only need to install these once. Delete these lines after that
# install.packages('reshape2')
# install.packages('raster')
# install.packages('ggplot2')

library(reshape2)
library(raster)
library(ggplot2)

remove(list = ls()); cat("\014")

# CHANGE FILEPATH ----
path <- "C:/RMS/006_scripts/R/riverAnalysis/"
source(paste0(path, "r/river_functions.R"))
# CHANGE FILEPATH ----

# PUT IN YOUR DATA HERE ----
curt <- paste0(path, 'data/points.csv')   # Data frame of sample points
dem  <- paste0(path, 'data/gis/dem.tif')  # Elevation raster
spce <- 2                                 # Spacing between sample points
span <- 100                               # Distance of the span on each side
# PUT IN YOUR DATA HERE ----

data <- getXsct(curt, dem, spce, span)
# Output file and check in QGIS
# write.csv(x = xsct, file = paste0(path, 'data/xsct.csv'),
#           row.names = F, quote = F)

xsct <- data[which(data$node == 22), ]
wse <- 148
colX <- 'xdst'; colZ <- 'elev'
wTop <- getTopWidth(wse, xsct, colX, colZ)
aXsc <- getXsctArea(wse, xsct, colX, colZ)
pWet <- getWetPerim(wse, xsct, colX, colZ)
rHyd <- getHydrRad(wse, xsct, colX, colZ)
dMax <- getMaxDepth(wse, xsct, colZ)
dAvg <- getMeanDepth(wse, xsct, colX, colZ)

# Plot a test cross section ----
# windows(12, 12)
# xsct <- data[which(data$node == 22), ]
# xsct$xdst <- xsct$xdst - min(xsct$xdst)
# ggplot(data = xsct, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
#   geom_line(color = 'brown')
# # Plot water surface
# wse <- 146.5
# wse <- createWSE(wse, xsct, colX, colZ)
# ggplot(data = xsct, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
#   geom_line(color = 'brown') + geom_line(data = wse, color = 'blue')
# Plot a test cross section ----

