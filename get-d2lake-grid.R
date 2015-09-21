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

############################
## set color ramp palette ##
## col.pal <- colorRampPalette(brewer.pal(9,"YlOrRd"))

#######################
## make grid centers ##
grid <- spsample(border, 1000, type = "regular")

#############################
## make spplotting objects ##
## bord.plot <- list("sp.polygons", border, fill = "white", plot.first = TRUE)
## locs.plot <- list("sp.points", dat, pch = 19, cex = .5)
## grid.plot <- list("sp.points", grid, pch = 19, col = "lightgrey", cex = .1)

################################
## calculate distance to lake ##
dist.grid <- c()
iter.grid <- c()
for(i in 1:nrow(coordinates(grid))){
  out <- Dist2Poly(grid[i,], lak, max.dist = 135000)
  dist.grid[i] <- out$distance
  iter.grid[i] <- out$iter
  cat(paste(i,dist.grid[i],iter.grid[i], sep ="\t"),"\n")
}

which(is.na(dist.grid))

out <- Dist2Poly(grid[807,], lak, max.dist = 135000)
dist.grid[807] <- out$distance
iter.grid[807] <- out$iter

write.table(dist.grid, "data/d2lake-grid-locations.dat", row.names = FALSE, col.names = FALSE)
write.table(coordinates(grid),"data/grid-coords", row.names = FALSE, col.names = FALSE)

##############################################
## make colored by distance to lake layouts ##
## dlocs.plot <- list("sp.points", dat, pch = 19, col = col.pal(10)[cut(dist,10)])
dgrid.plot <- list("sp.points", grid, pch = 19, col = col.pal(10)[cut(dist.grid,10)])


## spplot(lak[,1], col.regions = "transparent", colorkey = FALSE,
##        sp.layout = list(bord.plot, dlocs.plot))

spplot(lak[,1], col.regions = "transparent", colorkey = FALSE,
        sp.layout = list(bord.plot, dgrid.plot))

