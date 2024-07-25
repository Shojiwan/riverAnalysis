
# Data needs: 
# * Point vector (data frame) of locations with the following headers:
#   'cll2', 'east', 'nrth', 'dist'
# * Elevation data raster (DEM)
# Points file and DEM must have matching projections/CRN

library(reshape2); library(raster); library(ggplot2)
remove(list = ls()); cat("\014")
path <- "C:/RMS/006_scripts/R/riverAnalysis/"
source(paste0(path, "r/river_functions.R"))
curt <- paste0(path, 'data/points.csv')   # Data frame of sample points
dem  <- paste0(path, 'data/gis/dem.tif')  # Elevation raster
spce <- 2                                 # Spacing between sample points
span <- 100                               # Distance of the span on each side

data <- getXsct(curt, dem, spce, span)
# write.csv(x = xsct, file = paste0(path, 'data/xsct.csv'), 
#           row.names = F, quote = F)

