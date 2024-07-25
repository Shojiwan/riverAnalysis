# Returns a vector of unit distances orthogonal to segment angle 
orthog       <- function(angl) { # Angle is in degrees
  if (angl >= 360) {
    rots <- as.integer(angl / 360) # number of times around
    angl <- angl - 360 * rots
  }
  orth <- (angl + 90) / 360 * 2 * pi
  if (orth <= pi / 2) {            # Q1
    dX <- sin(orth); dY <- cos(orth) 
  } else if (orth <= pi) {         # Q2
    dX <- cos(orth - pi / 2); dY <- -sin(orth - pi / 2)
  } else if (orth <= 3 * pi / 2) { # Q3
    dX <- -sin(orth - pi); dY <- -cos(orth - pi)
  } else {                         # Q4
    dX <- -cos(orth - 3 * pi / 2); dY <- sin(orth - 3 * pi / 2)
  }
  return(c(x = round(dX, 3), y = round(dY, 3)))
}

# Define aspect as angle from true north in a clockwise direction
# Aspect will be base on the tangent of US and DS lines at the node
# Which is the same as the line between the US and DS nodes
getAspect <- function(curt) {
  crtn <- read.csv(curt)
  keep <- c('cll2', 'east', 'nrth', 'dist')
  crtn <- crtn[order(crtn$id, decreasing = T), 
               which(names(crtn) %in% keep)] # Goes from DS to US
  crtn$aspc <- NA
  for (i in 1 : nrow(crtn)) {
    if (i != 1 & i != nrow(crtn)) {
      nUS <- i - 1
      nDS <- i + 1
    } else if (i == 1) { # First node
      nUS <- i
      nDS <- i + 1
    } else { # Last node
      nUS <- i - 1
      nDS <- i
    }
    line <- crtn[c(nUS, nDS), ]
    dX <- line$east[2] - line$east[1] 
    dY <- line$nrth[2] - line$nrth[1]
    aspc <- atan(dX/dY) * 180 / pi
    if (dX > 0) {
      if (dY > 0) { # Q1
        crtn$aspc[i] <- aspc
      } else {      # Q2
        crtn$aspc[i] <- aspc + 180
      }
    } else {
      if (dY < 0) { # Q3
        crtn$aspc[i] <- aspc + 180
      } else {      # Q4
        crtn$aspc[i] <- aspc + 360
      }
    }
  }
  crtn$aspc <- round(crtn$aspc, 1)
  return(crtn)
}

# Returns cross section data (distance and elevation)
getXsct <- function(curt, dem, spce, span) {
  # Spce is spacing between points, span is one-sided xsct span from thalweg
  crtn <- read.csv(curt)
  rstr <- raster(dem)
  # Get node coordinates and stream aspect
  aspc <- getAspect(curt)
  pnts <- seq(-span, span, spce)
  for (i in 1 : nrow(aspc)) {
    orth <- orthog(angl = aspc$aspc[i])
    coor <- data.frame(node = aspc$cll2[i], ldst = aspc$dist[i], xdst = pnts, 
                       east = aspc$east[i] + orth[1] * pnts,
                       nrth = aspc$nrth[i] + orth[2] * pnts)
    if (i == 1) {xsct <- coor} else {xsct <- rbind(xsct, coor)}
  }
  # Extract elevations for cross section
  xsct$elev <- extract(rstr, xsct[, 4 : 5])
  xsct <- xsct[complete.cases(xsct$elev), ]
  return(xsct)
}

