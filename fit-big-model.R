rm(list=ls())
###############
## Libraries ##
library(sp)
library(rgdal)
library(rgeos)
library(gstat)
library(RColorBrewer)
library(classInt)
library(forecast)
library(nlme)

slam.it <- function(left,mat,right){
  t(left)%*%mat%*%right
}


##################
## read in data ##
shp    <- readOGR("data/michigan","michigan")
dat    <- read.table("data/select-stations-filled.dat")
coords <- read.table("data/select-locations.dat")
coordinates(coords) <- coords
proj4string(coords) <- proj4string(shp)
d2lake <- unlist(c(read.table("data/d2lake-select-locations.dat")))/1000


#########################################
## Reproject everything to UTM zone 16 ##
targetProj <- CRS("+proj=utm +zone=16 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
coords <- spTransform(coords,targetProj)

#################################
## make seasonal cosine vector ##
p      <- 365
offset <- -.4083
s      <- rep(cos(seq(-pi,pi, length.out = p) + offset),5)

###########################
## create model matrices ##
Y <- matrix(dat[1:1825,2], ncol = 1)
for(i in 3:73){Y <- rbind(Y, matrix(dat[1:1825,i], ncol = 1))}

X <- cbind(rep(coordinates(coords)[,1], each = 1825),
           rep(s,72),
           rep(d2lake, each = 1825))

X <- as.data.frame(cbind(X,X[,2]*X[,3]))

names(X) <- c("latitude","season","d2lake","seasonXd2lake")

pre.mod <- lm(Y ~ X$latitude + X$season + X$d2lake + X$seasonXd2lake)

################################
## estimate covariance matrix ##
arma.list <- list(rep(NA,72))
for(i in 1:72){
  arma.list[[i]] <- Arima(resid(pre.mod)[(1:1825)+(1825*(i-1))], c(3,0,0), include.mean = FALSE)
}

phi.mat <- matrix(NA, ncol = 3, nrow = 72)
sig.mat <- c()
for(i in 1:72){
  phi.mat[i,] <- coef(arma.list[[i]])
  sig.mat[i] <- arma.list[[i]]$sigma2
}

phi <- colMeans(phi.mat)
sig <- mean(sig.mat)

corr <- corARMA(phi, p = 3, q = 0, fixed = TRUE)
corr <- Initialize(corr, data = matrix(rep(0,1825), ncol = 1))
corr <- corMatrix(corr)
gam.0 <- sig/(1-phi[1]*corr[1,2] - phi[2]*corr[1,3] - phi[3]*corr[1,4])

GamMat <- gam.0*corr
GamInv <- chol2inv(chol(GamMat))

#############################################
## obtain generalized least squares beta's ##
X <- cbind(1,as.matrix(X))

row.cnt <- seq(from = 0, by = 1825, length.out = 72)+1

slam1.list <- list(rep(NA,72))
for(i in 1:72){
  slam1.list[[i]] <- slam.it(X[row.cnt[i]:(row.cnt[i]+1824),],GamInv,
                             X[row.cnt[i]:(row.cnt[i]+1824),])
}

slam2.list <- list(rep(NA,72))
for(i in 1:72){
  slam2.list[[i]] <- slam.it(X[row.cnt[i]:(row.cnt[i]+1824),],GamInv,
                             Y[row.cnt[i]:(row.cnt[i]+1824)])
}

gB <- solve(Reduce('+',slam1.list))%*%Reduce('+',slam2.list)

gB.se <- sqrt(diag(solve(Reduce('+',slam1.list))))

cbind(gB - gB.se*1.96, gB + gB.se*1.96)

W <- Y - X%*%gB

W.mat <- matrix(W,ncol = 72)

fitted.list <- list(rep(NA,72))
fitNOar.list <- list(rep(NA,72))
Y.list <- list(rep(NA,72))

for(j in 1:72){
  W1 <- matrix(ncol = 4,nrow = 0)
  for(i in 1:1822){
    W1 <- rbind(W1, c(W.mat[(4+i-1),j],W.mat[(3+i-1),j],W.mat[(2+i-1),j],W.mat[(1+i-1),j]))
  }
  
  fitted.list[[j]]  <- X[row.cnt[j]:(row.cnt[j]+1824),][4:1825,]%*%gB + W1[,2:4]%*%(phi)
  fitNOar.list[[j]] <- X[row.cnt[j]:(row.cnt[j]+1824),][4:1825,]%*%gB
  Y.list[[j]]       <- Y[row.cnt[j]:(row.cnt[j]+1824)][4:1825]
}

for(i in 1:72){
  png(paste("figures/big/fitted/station-",i,".png",sep = ""), width=12, height=6,
      units = "in", res = 100)
  plot(as.POSIXct(dat[,1])[4:1825],Y.list[[i]], type = "l", ylim = c(-25,40),
       main = paste("station",i), ylab = "",xlab = "")
  points(as.POSIXct(dat[,1])[4:1825],fitted.list[[i]],type = "l", col = "red")
  points(as.POSIXct(dat[,1])[4:1825],fitNOar.list[[i]], type = "l",col = "blue", lwd = 2)
  legend("topleft", c("Original Data","AR Fitted","Seasonal Component"),
         col = c("black","red","blue"), lty = c(1,1,1), bty = "n")
  dev.off()
}


for(i in 1:72){
  png(paste("figures/big/acf/station-",i,".png",sep = ""), width=6, height=6,
      units = "in", res = 100)
  acf(Y.list[[i]] -fitted.list[[i]], ylim = c(-.2,.8), main = paste("station",i, "residuals"))
  dev.off()
  png(paste("figures/big/pacf/station-",i,".png",sep = ""), width=6, height=6,
      units = "in", res = 100)
  pacf(Y.list[[i]] -fitted.list[[i]], ylim = c(-.2,.8), main = paste("station",i, "residuals"))
  dev.off()
}


## predict for time series ##
Y.time.hold <- read.table("data/select-time-holdout.dat")
p      <- 365
offset <- -.4083
s.pred <- cos(seq(-pi,pi, length.out = p) + offset)

for(i in 1:72){
  X.time.hold.1 <- cbind(1, rep(coordinates(coords)[i,1], 361),
                         s.pred[1:361],
                         rep(d2lake[i], 361))
  
  X.time.hold.1 <- cbind(X.time.hold.1,X.time.hold.1[,3]*X.time.hold.1[,4])
  
  W.time.hold.1        <- matrix(0,ncol = 3, nrow = 361)
  W.time.hold.1[1,]    <- (Y.list[[1]] - fitted.list[[1]])[1822:1820]
  W.time.hold.1[2,2:3] <- (Y.list[[1]] - fitted.list[[1]])[1822:1821]
  W.time.hold.1[3,3]   <- (Y.list[[1]] - fitted.list[[1]])[1822]
  
  
  Y.hold.pred.1 <- X.time.hold.1%*%gB + W.time.hold.1%*%phi
  
  png(paste("figures/big/forecast/station-",i,".png",sep = ""), width=12, height=6,
      units = "in", res = 100)
  plot(as.POSIXct(factor(c(as.character(dat[,1][4:1825]),as.character(Y.time.hold[,1])))),
       c(Y.list[[i]],Y.time.hold[,i+1]), type = "l", ylim = c(-25,40),
       main = paste("station",i), ylab = "",xlab = "")
  points(as.POSIXct(dat[,1])[4:1825],fitted.list[[i]],type = "l", col = "red")
  points(as.POSIXct(dat[,1])[4:1825],fitNOar.list[[i]], type = "l",col = "blue", lwd = 2)
  points(as.POSIXct(Y.time.hold[,1]),Y.hold.pred.1, type = "l", col = "orange",lwd = 3)
  legend("topleft", c("Original Data","AR Fitted","Seasonal Component","Forecast"),
         col = c("black","red","blue","orange"), lty = c(1,1,1,1), bty = "n")
  dev.off()
  
  png(paste("figures/big/forecast-zoom/station-",i,".png",sep = ""), width=12, height=6,
      units = "in", res = 100)
  plot(as.POSIXct(factor(c(as.character(dat[,1][4:1825]),as.character(Y.time.hold[,1])))),
       c(Y.list[[i]],Y.time.hold[,i+1]), type = "l", ylim = c(-25,40),
       xlim = c(as.POSIXct(dat[,1][1800]),as.POSIXct(Y.time.hold[,1][100])),
       main = paste("station",i), ylab = "",xlab = "")
  points(as.POSIXct(dat[,1])[4:1825],fitted.list[[i]],type = "l", col = "red")
  points(as.POSIXct(dat[,1])[4:1825],fitNOar.list[[i]], type = "l",col = "blue", lwd = 2)
  points(as.POSIXct(Y.time.hold[,1]),Y.hold.pred.1, type = "l", col = "orange",lwd = 3)
  legend("topleft", c("Original Data","AR Fitted","Seasonal Component","Forecast"),
         col = c("black","red","blue","orange"), lty = c(1,1,1,1), bty = "n")
  dev.off()
}

#################################################
## predict seasonal trend at holdout locations ##
Y.stat.hold <- read.table("data/select-stations-holdout.dat")
shp <- readOGR("data/michigan","michigan")
coords.stat.hold <- read.table("data/select-locations-holdout.dat")
coordinates(coords.stat.hold) <- coords.stat.hold
proj4string(coords.stat.hold) <- proj4string(shp)
d2lake.stat.hold <- unlist(c(read.table("data/d2lake-select-locations-holdout.dat")))/1000

coords.stat.hold <- spTransform(coords.stat.hold,targetProj)

p      <- 365
offset <- -.4083
s.pred <- rep(cos(seq(-pi,pi, length.out = p) + offset),6)[1:2186]

for(i in 1:10){
  X.stat.hold.1 <- cbind(1, rep(coordinates(coords.stat.hold)[i,1], 2186),
                         s.pred,
                         rep(d2lake.stat.hold[i], 2186))
  
  X.stat.hold.1 <- cbind(X.stat.hold.1,X.stat.hold.1[,3]*X.stat.hold.1[,4])
  
  Y.hold.pred.1 <- X.stat.hold.1%*%gB

  png(paste("figures/big/spatial-predict/station-",i,".png",sep = ""), width=12, height=6,
      units = "in", res = 100)
  plot(as.POSIXct(Y.stat.hold[,1]),Y.stat.hold[[i+1]], type = "l", ylim = c(-25,40),
       main = paste("station",i), ylab = "",xlab = "")
  points(as.POSIXct(Y.stat.hold[,1]),Y.hold.pred.1, type = "l",col = "red", lwd = 2)
  legend("topleft", c("Original Data","Seasonal Component"),
         col = c("black","red"), lty = c(1,1,1,1), bty = "n")
  dev.off()
}

##################################################
## predict seasonal component at grid locations ##
grid <- read.table("data/grid-coords") ## already in utm 16
d2lake.grid <- unlist(c(read.table("data/d2lake-grid-locations.dat")))/1000

p      <- 365
offset <- -.4083
s.pred <- rep(cos(seq(-pi,pi, length.out = p) + offset),6)[1:2186]

Y.grid <- matrix(NA,ncol = 2186,nrow = 994)
for(i in 1:994){
  X.grid.1 <- cbind(1, rep(grid[i,1], 2186),
                         s.pred,
                         rep(d2lake.grid[i], 2186))
  X.grid.1 <- cbind(X.grid.1,X.grid.1[,3]*X.grid.1[,4])
  Y.grid[i,] <- X.grid.1%*%gB
}

write.table(Y.grid,"data/Y-grid", row.names = FALSE, col.names = FALSE)
