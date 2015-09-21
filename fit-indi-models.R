rm(list=ls())
###############
## libraries ##
library(forecast)

##################
## read in data ##
dat <- read.table("data/select-stations-filled.dat")

#################################
## estimate seasonal component ##
## p <- 365
## try <- seq(-.4,-.42, length.out = 5000)
## R <- c()

## for(i in 1:length(try)){
##   offset <- try[i]
##   s <- rep(cos(seq(-pi,pi, length.out = p) + offset),5)
##   R[i] <- summary(lm(dat[,2] ~ s))$r.squared
## }

## try[which(R == max(R))]

p      <- 365
offset <- -.4083
s      <- rep(cos(seq(-pi,pi, length.out = p) + offset),5)

mod.list <- list(NA)
for(i in 2:ncol(dat)){
  mod.list[[i]] <- lm(dat[,i] ~ s)
}

arma.list  <- list(NA)
lower.list <- list(NA)
upper.list <- list(NA)
for(i in 2:ncol(dat)){
  arma.list[[i]]  <- Arima(resid(mod.list[[i]]), c(3,0,0), include.mean = FALSE)
  lower.list[[i]] <- c(arma.list[[i]]$coef - diag(sqrt(arma.list[[i]]$var.coef))*1.96)
  upper.list[[i]] <- c(arma.list[[i]]$coef + diag(sqrt(arma.list[[i]]$var.coef))*1.96)
}

phi1 <- c()
phi2 <- c()
phi3 <- c()
phi1.lo <- c()
phi2.lo <- c()
phi3.lo <- c()
phi1.up <- c()
phi2.up <- c()
phi3.up <- c()
for(i in 2:length(arma.list)){
  phi1[i-1] <- arma.list[[i]]$coef[1]
  phi2[i-1] <- arma.list[[i]]$coef[2]
  phi3[i-1] <- arma.list[[i]]$coef[3]
  phi1.lo[i-1] <- lower.list[[i]][1]
  phi2.lo[i-1] <- lower.list[[i]][2]
  phi3.lo[i-1] <- lower.list[[i]][3]
  phi1.up[i-1] <- upper.list[[i]][1]
  phi2.up[i-1] <- upper.list[[i]][2]
  phi3.up[i-1] <- upper.list[[i]][3]
}

b0 <- c()
b1 <- c()
b0.lo <- c()
b0.up <- c()
b1.lo <- c()
b1.up <- c()
for(i in 2:73){
  b0[i-1] <- coef(mod.list[[i]])[1]
  b1[i-1] <- coef(mod.list[[i]])[2]
  b0.lo[i-1] <- confint(mod.list[[i]])[1,1]
  b0.up[i-1] <- confint(mod.list[[i]])[1,2]
  b1.lo[i-1] <- confint(mod.list[[i]])[2,1]
  b1.up[i-1] <- confint(mod.list[[i]])[2,2]
}

png("figures/beta-plot.png", width=6, height=8, units = "in", res = 100)
plot(1:72,b0, ylim = c(-.1, 15), col = "blue", main = "beta's", xlab = "station",
     ylab = "", bty = "n")
abline(h = 0, lty = 2)
for(i in 1:72){segments(i,b0.lo[i],i,b0.up[i], col = "blue")}
points(1:72, b1, col = "red")
for(i in 1:72){segments(i,b1.lo[i],i,b1.up[i], col = "red")}
legend(x = 0, y = 4,c("intercept (b_0)","slope (b_1)"), pch = c(21,21),
       col = c("blue","red"), bty = "n")
dev.off()

png("figures/phi-plot.png", width=6, height=8, units = "in", res = 100)
plot(1:72,phi1, ylim = c(-.5,1), col = "blue", main = "Phi's", xlab = "station",
     ylab = "", bty = "n")
abline(h = 0, lty = 2)
for(i in 1:72){segments(i,phi1.lo[i],i,phi1.up[i], col = "blue")}
points(1:72,phi2, col = "red")
for(i in 1:72){segments(i,phi2.lo[i],i,phi2.up[i], col = "red")}
points(1:72,phi3, col = "forestgreen")
for(i in 1:72){segments(i,phi3.lo[i],i,phi3.up[i], col = "forestgreen")}
legend(x = 0, y = .5,c("phi_1","phi_2","phi_3"), pch = c(21,21,21),
       col = c("blue","red", "forestgreen"), bty = "n")
dev.off()

write.table(cbind(phi1,phi1.lo,phi1.up),"data/phi1-est",row.names = FALSE,col.names = FALSE)
write.table(cbind(phi2,phi2.lo,phi2.up),"data/phi2-est",row.names = FALSE,col.names = FALSE)
write.table(cbind(phi3,phi3.lo,phi3.up),"data/phi3-est",row.names = FALSE,col.names = FALSE)

write.table(cbind(b0,b0.lo,b0.up),"data/b0-est",row.names = FALSE,col.names = FALSE)
write.table(cbind(b1,b1.lo,b1.up),"data/b1-est",row.names = FALSE,col.names = FALSE)

for(i in 2:ncol(dat)){
  png(paste("figures/indi/seasonal/station-",i-1,".png",sep = ""),
      width=12, height=6, units = "in", res = 100)
  plot(as.POSIXct(dat[,1]),dat[,i], type = "l", col = "grey", main = paste("station",i-1),ylab = "")
  points(as.POSIXct(dat[,1]),fitted(mod.list[[i]]), type = "l", col = "red", lwd = 2)
  dev.off()
  
  png(paste("figures/indi/residual/station-",i-1,".png",sep = ""),
      width=12, height=6, units = "in", res = 100)
  plot(resid(mod.list[[i]]), type = "l",ylab = "")
  abline(h = 0, col = "red")
  dev.off()

  png(paste("figures/indi/pre-acf/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  acf(resid(mod.list[[i]]), ylim = c(-.2,.8), main = "ACF pre AR(3)",ylab = "")
  dev.off()

  png(paste("figures/indi/pre-pacf/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  pacf(resid(mod.list[[i]]), ylim = c(-.2,.8), main = "PACF pre AR(3)",ylab = "")
  dev.off()

  png(paste("figures/indi/pre-qq/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  qqnorm(resid(mod.list[[i]]), main = "QQ plot pre AR(3)",ylab = "")
  qqline(resid(mod.list[[i]]), col = "red")
  dev.off()
  
  png(paste("figures/indi/post-acf/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  acf(resid(arma.list[[i]]), ylim = c(-.2,.8), main = "ACF post AR(3)",ylab = "")
  dev.off()

  png(paste("figures/indi/post-pacf/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  pacf(resid(arma.list[[i]]), ylim = c(-.2,.8), main = "PACF post AR(3)",ylab = "")
  dev.off()
  
  png(paste("figures/indi/post-qq/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  qqnorm(resid(arma.list[[i]]), main = "QQ plot post AR(3)",ylab = "")
  qqline(resid(arma.list[[i]]), col = "red")
  dev.off()
}

for(i in 2:ncol(dat)){
  png(paste("figures/indi/post-acf-long-lag/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  acf(resid(arma.list[[i]]), ylim = c(-.2,.8), main = "ACF post AR(3)",ylab = "", lag = 750)
  abline(v = 365, lty = 2, col = "lightgrey")
  abline(v = 730, lty = 2, col = "lightgrey")
  dev.off()
  
  png(paste("figures/indi/post-pacf-long-lag/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  pacf(resid(arma.list[[i]]), ylim = c(-.2,.8), main = "PACF post AR(3)",ylab = "", lag = 750)
  abline(v = 365, lty = 2, col = "lightgrey")
  abline(v = 730, lty = 2, col = "lightgrey")
  dev.off()
  
  png(paste("figures/indi/pre-acf-long-lag/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  acf(resid(mod.list[[i]]), ylim = c(-.2,.8), main = "ACF pre AR(3)",ylab = "", lag = 750)
  abline(v = 365, lty = 2, col = "lightgrey")
  abline(v = 730, lty = 2, col = "lightgrey")
  dev.off()
  
  png(paste("figures/indi/pre-pacf-long-lag/station-",i-1,".png",sep = ""),
      width=6, height=6, units = "in", res = 100)
  pacf(resid(mod.list[[i]]), ylim = c(-.2,.8), main = "PACF pre AR(3)",ylab = "", lag = 750)
  abline(v = 365, lty = 2, col = "lightgrey")
  abline(v = 730, lty = 2, col = "lightgrey")
  dev.off()
}
