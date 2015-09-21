rm(list=ls())
###############
## Libraries ##
library(sp)
library(rgdal)
library(gstat)
library(RColorBrewer)
library(classInt)
library(rgeos)
library(animation)

#######################################
## set ploting resolution parameters ##
divi <- 50    ## control number of color breaks in idws
reso <- 10000 ## control number grid cells in idws
leftmar <- .1 ## control additional left margin (range from 0 to 1 prop increase)

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

b0 <- read.table("data/b0-est")
b1 <- read.table("data/b1-est")

#########################################
## Reproject everything to UTM zone 16 ##
targetProj <- CRS("+proj=utm +zone=16 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
shp <- spTransform(shp,targetProj)
border <- spTransform(border,targetProj)
lak <- spTransform(lak,targetProj)
dat <- spTransform(dat,targetProj)

coordinates(b0) <- coordinates(dat)
coordinates(b1) <- coordinates(dat)
proj4string(b0) <- proj4string(dat)
proj4string(b1) <- proj4string(dat)

##########################
## make prediction grid ##
grid <- spsample(shp,n = reso, type = "regular")
gridded(grid) <- TRUE

####################
## set color ramp ##
colpal.b0 <- colorRampPalette(c("blue","red"))
colpal.b1 <- colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])

##########################
## make sp.layout lists ##
lak.plot    <- list("sp.polygons",lak   , fill = "azure2", plot.first = TRUE)
state.plot  <- list("sp.polygons",shp   , col = "grey", fill = "white")
loc.plot    <- list("sp.points"  ,dat   , pch = 19, col = "red")
border.plot <- list("sp.polygons",border, lwd = 1.5)
border.plot.fill <- list("sp.polygons",border, lwd = 1.5, fill = "white")

###################################
## Make plot bounding box limits ##
xlim <- bbox(gEnvelope(shp))[1,]
ylim <- bbox(gEnvelope(shp))[2,]

xlim[1] <- xlim[1]-(xlim[2]-xlim[1])*leftmar

####################
## plot the lakes ##
plot.lak <- spplot(lak[,'gid'], col.regions = "azure2",
                   colorkey = FALSE, sp.layout = list(state.plot,border.plot,loc.plot))


#############
## Plot b0 ##
b0.idw <- idw(b0@data[,1] ~ 1, b0, grid)
b0.class <- classIntervals(b0.idw$var1.pred, divi, style = "equal")

legLabs <- levels(cut(b0@data[,1],5, dig.lab = 1))
legLabs <- gsub(","," -- ", legLabs)
legLabs <- gsub("]","", legLabs)
legLabs <- gsub("\\(","", legLabs)

b0.plot <- list("sp.points",b0, col = colpal.b0(5)[cut(b0@data[,1],5)], pch = 19)

plot.b0 <- spplot(b0[,1], sp.layout = list(lak.plot,border.plot.fill,state.plot,border.plot,b0.plot),
                  col.regions = colpal.b0(5), cex = 1,
                  main = "Beta_0 Estimates", xlim = xlim, ylim = ylim, legendEntries = legLabs)
names(plot.b0$legend) <- "inside"
plot.b0$legend$inside$x <- .0
plot.b0$legend$inside$y <- .2

plot.b0.idw <- spplot(b0.idw[,1], col.regions = colpal.b0(divi),at = b0.class$brks,
                      sp.layout = list(lak.plot,border.plot.fill,border.plot),
                      main = "Beta_0 Estimates",xlim = xlim, ylim = ylim)


############################
## Plot residual variance ##
b1.idw <- idw(b1@data[,1] ~ 1, dat, grid)
b1.class <- classIntervals(b1.idw$var1.pred, divi, style = "quantile")

legLabs <- levels(cut(b1@data[,1],5, dig.lab = 3))
legLabs <- gsub(","," -- ", legLabs)
legLabs <- gsub("]","", legLabs)
legLabs <- gsub("\\(","", legLabs)

b1.plot <- list("sp.points",b1, col = colpal.b0(5)[cut(b1@data[,1],5)], pch = 19)

plot.b1 <- spplot(b1[,1], sp.layout = list(lak.plot,border.plot.fill,state.plot,border.plot,b1.plot),
                  col.regions = colpal.b0(5), cex = 1,
                  main = "Beta_1 Estimates", xlim = xlim, ylim = ylim, legendEntries = legLabs)
names(plot.b1$legend) <- "inside"
plot.b1$legend$inside$x <- .0
plot.b1$legend$inside$y <- .2

plot.b1.idw <- spplot(b1.idw[,1], col.regions = colpal.b0(divi),at = b1.class$brks,
                       sp.layout = list(lak.plot,border.plot.fill,border.plot),
                       main = "Beta_1 Estimates", xlim = xlim, ylim = ylim)


###########################
## plot grid predictions ##
Grid <- read.table("data/grid-coords")
Y.Grid <- read.table("data/Y-grid")
coordinates(Y.Grid) <- Grid
proj4string(Y.Grid) <- proj4string(shp)

dates <- read.table("data/select-stations-holdout.dat")[,1]

predpal <- colorRampPalette(brewer.pal(11,"Spectral")[11:1])

Y.Grid.class <- classIntervals(unlist(c(Y.Grid@data)), divi, style = "equal")

saveLatex({
  for(i in 1:2186){
    t1.idw   <- idw(Y.Grid@data[,i] ~ 1, Y.Grid, grid)
    plot.t1 <- spplot(t1.idw[,1], col.regions = predpal(divi),at = Y.Grid.class$brks,
                      sp.layout = list(lak.plot,border.plot.fill,border.plot),
                      main = as.character(as.POSIXct(dates[i])), xlim = xlim, ylim = ylim)
    print(plot.t1)
  }
},pdflatex = NULL)
###################
## Plot the maps ##
png("figures/maps/locations.png", width=8, height=6, units = "in", res = 100)
plot.lak
dev.off()


png("figures/maps/b0.png", width=6, height=6, units = "in", res = 100)
plot.b0
dev.off()


png("figures/maps/b0-idw.png", width=6, height=6, units = "in", res = 100)
plot.b0.idw
dev.off()


png("figures/maps/b1.png", width=6, height=6, units = "in", res = 100)
plot.b1
dev.off()


png("figures/maps/b1-idw.png", width=6, height=6, units = "in", res = 100)
plot.b1.idw
dev.off()
