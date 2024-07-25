
# Data needs: 
# * Point vector (data frame) of locations with the following headers:
#   'cll2', 'east', 'nrth', 'dist'
# * Elevation data raster (DEM)
# Points file and DEM must have matching projections/CRN

library(reshape2); library(raster); library(ggplot2)
remove(list = ls()); cat("\014")

path <- "C:/RMS/005_model/008_MD/03_models/002_tuflow/r/riverAnalysis/"
source(paste0(path, "river_functions.R"))
curt <- paste0(path, 'points.csv')        # Data frame of sample points
dem  <- paste0(path, 'dem.tif')           # Elevation raster
spce <- 2                                 # Spacing between sample points
span <- 100                               # Distance of the span on each side

xsct <- getXsct(curt, dem, spce, span)
write.csv(x = xsct, file = paste0(path, 'xsct.csv'), row.names = F, quote = F)

temp <- xsct[which(xsct$node == 22), ]
windows(12, 12)
ggplot(data = temp, aes(x = xdst, y = elev)) + geom_point() + geom_line()

