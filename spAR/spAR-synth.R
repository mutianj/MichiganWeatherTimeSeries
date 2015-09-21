rm(list=ls())
###############
## libraries ##
library(MASS)
## libraries ##
###############

source("functions/bayesAR.R")

## synthetic example ##
len <- 250
ARcov.1 <- genAR(c(.7,-.4), 3, len)
ARcov.2 <- genAR(c(.6,-.5), 3, len)
ARcov.3 <- genAR(c(.5,-.5), 3, len)
ARcov.4 <- genAR(c(.6,-.3), 3, len)
ARcov.5 <- genAR(c(.7,-.5), 3, len)

Y <- c(mvrnorm(1,rep(0,len),ARcov.1), mvrnorm(1,rep(0,len),ARcov.2),
       mvrnorm(1,rep(0,len),ARcov.3), mvrnorm(1,rep(0,len),ARcov.4),
       mvrnorm(1,rep(0,len),ARcov.5))

Y.1 <- Y[1:len]
Y.2 <- Y[(len+1):(2*len)]
Y.3 <- Y[(2*len+1):(3*len)]
Y.4 <- Y[(3*len+1):(4*len)]
Y.5 <- Y[(4*len+1):(5*len)]

par(mfrow = c(5,2))
ylim = c(-.6,.6)
plot(Y.1 ,type = 'l');pacf(Y.1, ylim = ylim)
plot(Y.2 ,type = 'l');pacf(Y.2, ylim = ylim)
plot(Y.3 ,type = 'l');pacf(Y.3, ylim = ylim)
plot(Y.4 ,type = 'l');pacf(Y.4, ylim = ylim)
plot(Y.5 ,type = 'l');pacf(Y.5, ylim = ylim)

iter = 1000
mod <- bayesAR(Y,phi.1 = .5, phi.2 = -.5, sig = 1,
               phi.tune = .05, sig.tune = .05, iter = iter, series = 5)
burn.in <- 100

est.pars <- rowMeans(mod[,-c(1:burn.in)])

par(mfrow = c(3,2))
plot(mod[1,], type = "l", main = "phi 1")
plot(density(mod[1,(burn.in+1):iter]), main = "phi 1")
plot(mod[2,], type = "l", main = "phi 2")
plot(density(mod[2,(burn.in+1):iter]), main = "phi 2")
plot(mod[3,], type = "l", main = "sigma^2")
plot(density(mod[3,(burn.in+1):iter]), main = "sigma^2")

t(apply(mod[,-c(1:burn.in)], 1, quantile, probs = c(.5,.025, .975)))


F.1 <- c(NA,NA)
F.2 <- c(NA,NA)
F.3 <- c(NA,NA)
F.4 <- c(NA,NA)
F.5 <- c(NA,NA)
for(i in 3:len){
  F.1[i] <- Y.1[i-1]*est.pars[1] + Y.1[i-2]*est.pars[2]
  F.2[i] <- Y.2[i-1]*est.pars[1] + Y.2[i-2]*est.pars[2]
  F.3[i] <- Y.3[i-1]*est.pars[1] + Y.3[i-2]*est.pars[2]
  F.4[i] <- Y.4[i-1]*est.pars[1] + Y.4[i-2]*est.pars[2]
  F.5[i] <- Y.5[i-1]*est.pars[1] + Y.5[i-2]*est.pars[2]
}

R.1 <- Y.1 - F.1
R.2 <- Y.2 - F.2
R.3 <- Y.3 - F.3
R.4 <- Y.4 - F.4
R.5 <- Y.5 - F.5

par(mfrow = c(5,1))
plot(Y.1 ,type = 'l');points(F.1, col = "red", type = "l")
plot(Y.2 ,type = 'l');points(F.2, col = "red", type = "l")
plot(Y.3 ,type = 'l');points(F.3, col = "red", type = "l")
plot(Y.4 ,type = 'l');points(F.4, col = "red", type = "l")
plot(Y.5 ,type = 'l');points(F.5, col = "red", type = "l")

par(mfrow = c(5,2))
plot(R.1 ,type = 'l');plot(density(R.1, na.rm = TRUE))
plot(R.2 ,type = 'l');plot(density(R.2, na.rm = TRUE))
plot(R.3 ,type = 'l');plot(density(R.3, na.rm = TRUE))
plot(R.4 ,type = 'l');plot(density(R.4, na.rm = TRUE))
plot(R.5 ,type = 'l');plot(density(R.5, na.rm = TRUE))

par(mfrow = c(5,2))
ylim = c(-.6,.6)
pacf(Y.1, main = "Actual", ylim = ylim);pacf(R.1[-c(1,2)], main = "Residual", ylim = ylim)
pacf(Y.2, main = "Actual", ylim = ylim);pacf(R.2[-c(1,2)], main = "Residual", ylim = ylim)
pacf(Y.3, main = "Actual", ylim = ylim);pacf(R.3[-c(1,2)], main = "Residual", ylim = ylim)
pacf(Y.4, main = "Actual", ylim = ylim);pacf(R.4[-c(1,2)], main = "Residual", ylim = ylim)
pacf(Y.5, main = "Actual", ylim = ylim);pacf(R.5[-c(1,2)], main = "Residual", ylim = ylim)
