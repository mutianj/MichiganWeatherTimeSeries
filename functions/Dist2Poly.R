###################################################################
## function to calculate distance from a point to a polygon      ##
## within a specified tolerance.                                 ##
##                                                               ##
## pt = SpatialPointsDataFrame containing only one point         ##
##                                                               ##
## poly = SpatialPolygonsDataFrame containing polygons           ##
## max.dist = The maximum distance to check.                     ##
##      Set max.dist to a positive number if pt is outside poly  ##
##      Set max.dist to a negative number if pt is inside poly   ##
## iter = maximum number of iterations to go through             ##
## tol = tolerance threshold                                     ##
##                                                               ##
## depends on 'rgeos'                                            ##
##                                                               ##
###################################################################

getWidth <- function(range){(range[2]-range[1])/2 + range[1]}

Dist2Poly <- function(pt, poly, max.dist, iter = 25, tol = .1){
  tolCheck <- TRUE
  range <- sort(c(0,max.dist))
  width <- 1
  check <- is.na(overlay(pt, poly)) 
  if(max.dist > 0 && check == FALSE){
    width <- 0
  }else if(max.dist < 0 && check == TRUE){
    width <- 0
  }
    
  i <- 0
  while(i < iter && tolCheck == TRUE && width != 0){
    i <- i + 1
    width <- getWidth(range)
    buff  <- gBuffer(poly, width = width)
    
    if(is.na(overlay(pt, buff)) == FALSE){ 
      ## if its in the buffer
      range <- c(range[1], width)
      tolBuff <- gBuffer(poly, width = (width - tol))
      
      if(is.na(overlay(pt,tolBuff)) == TRUE){tolCheck <- FALSE}
      
    }else{
      ## if its not in the buffer
      range <- c(width, range[2])
      tolBuff <- gBuffer(poly, width = (width + tol))
      
      if(is.na(overlay(pt,tolBuff)) == FALSE){tolCheck <- FALSE}
    }
  }
  if(i == iter){print("Warning - Max iterations reached")}
  width  <- abs(width)
  output <- list(distance = width, iter = i)
  return(output)
}
###################################################################                

