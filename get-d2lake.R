rm(list=ls())
###############
## Libraries ##
library(sp)
library(rgdal)
library(rgeos)
library(gstat)
library(RColorBrewer)
library(classInt)
source("functions/Dist2Poly.R")

##################
## read in data ##
shp    <- readOGR("data/michigan","michigan")
border <- gUnaryUnion(shp)
lak    <- readOGR("data/great_lakes","great_lakes")
proj4string(lak) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0
+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +over +no_defs")
dat <- read.table("data/select-locations.dat", header = FALSE)
coordinates(dat) <- dat
proj4string(dat) <- proj4string(shp)
lak <- spTransform(lak, CRS(proj4string(shp)))

#########################################
## Reproject everything to UTM zone 16 ##
targetProj <- CRS("+proj=utm +zone=16 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
shp    <- spTransform(shp, targetProj)
dat    <- spTransform(dat, targetProj)
lak    <- spTransform(lak, targetProj)
border <- spTransform(border, targetProj)

################################
## calculate distance to lake ##
dist <- c()
iter <- c()
for(i in 1:nrow(dat)){
  out <- Dist2Poly(dat[i,], lak, max.dist = 135000)
  dist[i] <- out$distance
  iter[i] <- out$iter
  cat(paste(i,dist[i],iter[i], sep ="\t"),"\n")
}

write.table(dist, "data/d2lake-select-locations.dat", row.names = FALSE, col.names = FALSE)
