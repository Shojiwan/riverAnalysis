# Returns a vector of unit distances orthogonal to segment angle 
orthog <- function(angl) { # Angle is in degrees
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

# Creates water surface points for plotting
createWSE <- function(wse, xsct, colX, colZ) {
  xCol <- which(names(xsct) == colX)
  zCol <- which(names(xsct) == colZ)
  surf <- interpBanks(xsct, wse, colX, colZ)
  surf <- surf[which(is.na(surf$node)), ]
  naxs <- surf[1, ]
  naxs[, zCol] <- NA
  if (nrow(surf) > 2) {
    nbrk <- nrow(surf) / 2 - 1 # Number of section breaks
    for (n in 1 : nbrk) {
      naxs[, xCol]   <- mean(c(surf[2 * n, xCol], surf[2 * n + 1, xCol]))
      surf <- rbind(surf, naxs)
    }
  }
  return(surf)
}

# Takes arguments of water surface elevation and cross section and returns
# the same xsct DF, with added rows with a right/left banks (and a 'type' column)
interpBanks <- function(wse, xsct, colX, colZ) {
  xCol <- which(names(xsct) == colX); zCol = which(names(xsct) == colZ)
  if (all(wse <= xsct[, zCol])) return(NULL)
  xsct$near <- xsct[, zCol] - wse
  xsct$type <- 'GRND'
  if (any(c(xsct$near[1], xsct$near[nrow(xsct)]) < 0)) {
    cat(paste0('Error: The cross section is completely inundated; unable to ',
               'calculate channel dimesions.'))
    return(NULL)
  }
  # Go through and count the number of regions of inundation (indt)
  cntr <- 0 # counter to tally number of instances of dry to wet/wet to dry
  indt <- list()
  for (i in 2 : (nrow(xsct))) {
    if (xsct$near[i - 1] > 0 & xsct$near[i] <= 0) {         # Start of inundation
      cntr <- cntr + 1
      tmpI <- i
    } else if (xsct$near[i - 1] <= 0 & xsct$near[i] <= 0) { # Inundated
      tmpI <- append(tmpI, i) 
    } else if (xsct$near[i - 1] <= 0 & xsct$near[i] > 0) {  # End of inundation
      indt[[cntr]] <- tmpI
    } 
  }
  # Interate through inundated sections and interpolate the bank locations
  for (i in 1 : cntr) {
    temp <- xsct[1 : 2, ]
    for (j in 1 : length(temp)) temp[, j] <- NA
    lBnk <- xsct[c(min(indt[[i]]) - 1, min(indt[[i]])), ]
    rBnk <- xsct[c(max(indt[[i]]), max(indt[[i]]) + 1), ]
    temp[1, xCol] <- lBnk[1, xCol] + (lBnk[2, xCol] - lBnk[1, xCol]) * 
      (wse - lBnk[1, zCol]) / (lBnk[2, zCol] - lBnk[1, zCol])
    temp[2, colX] <- rBnk[1, xCol] + (rBnk[2, xCol] - rBnk[1, xCol]) * 
      (wse - rBnk[1, zCol]) / (rBnk[2, zCol] - rBnk[1, zCol])
    temp[1, zCol] <- temp[2, zCol] <- wse
    temp$type[1] <- paste0('LB', ifelse(cntr < 10, '0', ''), i)
    temp$type[2] <- paste0('RB', ifelse(cntr < 10, '0', ''), i)
    xsct <- rbind(xsct, temp)
  }
  xsct <- xsct[order(xsct[, xCol]), -which(names(xsct) == 'near')]
  return(xsct)
}

# For these functions we assume we pass the water surface elevation and cross
# sections (maybe flow) and these functions will calculate those values based
# on interim values derived from the previous two functions.
getTopWidth <- function(wse, xsct, colX, colZ) {
  # Interpolate banks
  xsct <- interpBanks(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  xCol <- which(names(xsct) == colX); zCol <- which(names(xsct) == colZ)
  # Count the number of inundated sections and sum their lengths
  bnks <- which(substr(xsct$type, 1, 2) %in% c('LB', 'RB'))
  for (n in 1 : (length(bnks) / 2)) {
    tmpW <- xsct[bnks[(n - 1) * 2 + 2], xCol] - xsct[bnks[(n - 1) * 2 + 1], xCol]
    if (n == 1) {wdth <- tmpW} else {wdth <- wdth + tmpW}
  }
  return(wdth)
}

getXsctArea <- function(wse, xsct, colX, colZ) {
  # Interpolate banks and cull ground above wse
  xsct <- interpBanks(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  xCol <- which(names(xsct) == colX); zCol = which(names(xsct) == colZ)
  xsct <- xsct[which(xsct[, zCol] <= wse), ]
  xsct$dpth_m <- max(xsct[, zCol]) - xsct[, zCol] # Max depth
  xsct$dpth_A <- 0                                # Average depth
  xsct$lgth <- 0                                  # Section length
  for (n in 2 : nrow(xsct)) {
    xsct$dpth_A[n] <- mean(c(xsct$dpth_m[n], xsct$dpth_m[n - 1]))
    xsct$lgth[n]   <- xsct[n, xCol] - xsct[n - 1, xCol]
  }
  area <- sum(xsct$dpth_A * xsct$lgth)
  return(area)
}

getWetPerim <- function(wse, xsct, colX, colZ) {
  # Interpolate banks and cull ground above wse
  xsct <- interpBanks(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  xCol <- which(names(xsct) == colX); zCol = which(names(xsct) == colZ)
  xsct <- xsct[which(xsct[zCol] <= wse), ]
  # Count and process the number of inundated sections
  nsct <- length(which(substr(xsct$type, 1, 2) == 'RB'))
  for (i in 1 : nsct) {
    indx <- (which(xsct$type == paste0('LB', ifelse(i < 10, '0', ''), i)) :
               which(xsct$type == paste0('RB', ifelse(i < 10, '0', ''), i)))
    wTmp <- calcDist(x = xsct[indx, xCol], y = xsct[indx, zCol])
    if (i == 1) {pWet <- wTmp} else {pWet <- pWet + wTmp}
  }
  return(pWet)
}

getHydrRad  <- function(wse, xsct, colX, colZ) {
  # X-sect area of the flow channel to the length of the wetted perimeter
  # Interpolate banks and cull ground above wse
  # This gets tricky. tmpX is the modified DF truncated to the banks
  tmpX <- interpBanks(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  xCol <- which(names(xsct) == colX); zCol = which(names(xsct) == colZ)
  tmpX <- tmpX[which(tmpX[, zCol] <= wse), ]
  pWet <- getWetPerim(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  aXsc <- getXsctArea(xsct = xsct, wse = wse, colX = colX, colZ = colZ)
  rHyd <- aXsc / pWet
  return(rHyd)
}

getMaxDepth <- function(wse, xsct, colZ) {
  dMax <- wse - min(xsct[, which(names(xsct) == colZ)], na.rm = T)
  return(dMax)
}

getMeanDepth <- function(wse, xsct, colX, colZ) {
  xCol <- which(names(xsct) == colX)
  zCol <- which(names(xsct) == colZ)
  tmpX <- interpBanks(wse, xsct, colX, colZ)
  tmpX <- tmpX[which(tmpX[, zCol] <= wse), ]
  tmpX <- tmpX[which(tmpX$type == 'GRND'), ]
  dAvg <- mean(wse - tmpX[, zCol])
  return(dAvg)
}

calcDist     <- function(x, y) {
  if (length(x) != length(y)) cat('Please provide pairwise x-y data of equal length')
  sum <- 0
  for (i in 2 : length(x)) {
    sum <- sum + sqrt((x[i] - x[i - 1])^2 + (y[i] - y[i - 1])^2)
  }
  return(sum)
}

# getManningsN <- function(wse, xsct, colX, colZ,
#                          q = NULL, S = NULL) {
#   # Q = V x A = 1 / n x A x R^(2/3) x S^(1/2) ==> n = 1 / Q x A x R^(2/3) x S^(1/2)
#   area <- getXsctArea(wse = wse, xsct = xsct, colX = colX, colZ = colZ)
#   rHyd <- getHydrRad(wse = wse, xsct = xsct, colX = colX, colZ = colZ)
#   n    <- (1 / q) * area * rHyd^(2/3) * sqrt(S)
#   return(n)
# }
# 
# 
# depth_solver <- function(Q = NULL, n = 0.03, S = 0.001, xsct = NULL,
#                          colX = NULL, colZ = NULL, tolr = 0.001) {
#   # takes cross section, known flow, Mannings n and longitudinal slope and
#   # returns the depth of water for those conditions
#   # Q = n^-1 * A * R^(2/3) * S^0.5 ===> A^(5/3) / P^(2/3) = n * Q / sqrt(S)
#   RHS  <- n * Q / sqrt(S)
#   xCol = which(names(xsct) == colX); zCol <- which(names(xsct) == colZ)
#   zMin <- min(xsct[, zCol], na.rm = T); zMax <- max(xsct[, zCol], na.rm = T)
#   # Half of the xsct max depth
#   dpth <- (zMax - zMin) / 2 # Starting depth (this will change throughout)
#   dDlt <- (zMax - zMin) / 4 # Starting adjustment interval (will change too)
#   repeat {
#     # Calculate area and wetted perimeter
#     wse  <- dpth + zMin
#     area <- getXsctArea(wse = wse, xsct = xsct, colX = colX, colZ = colZ)
#     pWet <- getWetPerim(wse = wse, xsct = xsct, colX = colX, colZ = colZ)
#     LHS  <- area^(5/3) / pWet^(2/3)
#     if (abs(LHS - RHS) < tolr) break
#     if (LHS > RHS) {dpth <- dpth - dDlt} else {dpth <- dpth + dDlt}
#     dDlt <- dDlt / 2
#   }
#   return(dpth)
# }



