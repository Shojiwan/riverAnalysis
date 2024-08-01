
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

# Run bench index and/or width/depth using wTop and dMax (from below) 
bench_index <- xxxxx # (Riley, 1977) 
width_depth <- yyyyy # (Wolman, 1955)

colX <- 'xdst'; colZ <- 'elev'
node <- unique(data$node)
for(j in 1 : length(node)) {
  temp <- data[which(data$node == node[j]), ]
  for(i in 1 : length(wse)) {
    wTop <- getTopWidth(wse[i], temp, colX, colZ)
    aXsc <- getXsctArea(wse[i], temp, colX, colZ)
    dMax <- getMaxDepth(wse[i], temp, colZ)
  }
}

# Plot a test cross section ----
# windows(12, 12)
# xsct <- data[which(data$node == 22), ]
# xsct$xdst <- xsct$xdst - min(xsct$xdst)
# ggplot(data = xsc1, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
#   geom_line(color = 'brown')
# # Plot water surface
# wse <- 146.5
# wse <- createWSE(wse, xsct, colX, colZ)
# ggplot(data = xsct, aes(x = xdst, y = elev)) + geom_point() + theme_bw() +
#   geom_line(color = 'brown') + geom_line(data = wse, color = 'blue')
# Plot a test cross section ----

