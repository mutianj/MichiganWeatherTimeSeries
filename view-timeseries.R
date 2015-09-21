rm(list=ls())
###############
## libraries ##
library(rgdal)
library(rgeos)

##################
## read in data ##
dat <- read.table("data/select-stations.dat")
mn  <- read.table("data/mean.dat")
sn  <- read.table("data/seasonal.dat")
res <- read.table("data/residual.dat")
loc <- read.table("data/select-locations.dat")
shp <- readOGR("data/michigan","michigan")
shp <- gUnaryUnion(shp)

#########################
## start plot sequence ##
for(i in 1:72){
  par(ask=T,par(mar = c(5,4,4,2)))
  layout(matrix(c(1,1,1,2,2,2,3,4,5), 3, 3, byrow=TRUE))
  plot(as.POSIXct(dat[,1]), dat[,(i+1)], type = "l", main = paste("Station",i),
       xlab = "date",ylab = "temperature", bty = "n", ylim = c(-30,30))
  lines(as.POSIXct(dat[,1]),mn[,(i+1)] + sn[,(i+1)], type = "l", col = "red")

  plot(as.POSIXct(dat[,1]), res[,(i+1)], type = "l", main = "",
       xlab = "date",ylab = "temperature residual", bty = "n", ylim = c(-15,15))
  abline(h = 0, col = "red")

  hist(res[,(i+1)], xlim = c(-15,15), main = "Histogram", xlab = "Residuals")
  
  qqnorm(res[,(i+1)], bty = "n");qqline(res[,(i+1)], col = "red")

  par(mar = c(1,2,2,2))
  plot(shp)
  title(main = "Location")
  points(loc, pch = 19, cex = .3)
  points(loc[i,], col = "red", pch = 19, cex = .75)
}
