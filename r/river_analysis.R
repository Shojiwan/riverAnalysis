
install.packages('reshape2')
install.packages('raster')
install.packages('ggplot2')

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

# Plot a test cross section ----
# windows(12, 12)
# xsct <- data[which(data$node == 22), ]
# xsct$xdst <- xsct$xdst - min(xsct$xdst)
# # ggplot(data = xsct, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
# #   geom_line(color = 'brown')
# # Plot water surface
# wse <- 146.5
# wse <- createWSE(xsct = xsct, wse = wse, colX = 'xdst', colZ = 'elev')
# ggplot(data = xsct, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
#   geom_line(color = 'brown') + geom_line(data = wse, color = 'blue')
# Plot a test cross section ----

# Calculate hydraulic geometry and other features
# getTopWidth
# getXsctArea
# getWetPerim
# getHydrRad
# getManningsN
# depth_solver

