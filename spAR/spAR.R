rm(list=ls())

###############
## Functions ##
source("functions/bayesAR.R")

##################
## read in data ##
AR2       <- read.table("models/AR2")
coords    <- read.table("data/select-locations.dat")
res       <- read.table("data/residual.dat")

## estimate missing values with mean + seasonal component
for(i in 2:73){
  res[which(is.na(res[,i]) == TRUE),i] <- 0
}

#####################
## Fit Bayesian AR ##
iter = 1000
mod <- bayesAR(res[1:500,2:73],                    ## data (one time series for each column)
               phi.1 = .77, phi.2 = -.12, sig = 9, ## starting values
               phi.tune = .013, sig.tune = .013,   ## tuning values
               iter = iter)                        ## number of MCMC samples
burn.in = 100

t(apply(mod[,-c(1:burn.in)], 1, quantile, probs = c(.5,.025, .975)))

colMeans(AR2)
apply(AR2,2,range)

write.table(mod,"mod",col.names = FALSE, row.names = FALSE)

par(mfrow = c(3,2))
plot(mod[1,], type = "l", main = "phi 1")
plot(density(mod[1,(burn.in+1):iter]), main = "phi 1")
plot(mod[2,], type = "l", main = "phi 2")
plot(density(mod[2,(burn.in+1):iter]), main = "phi 2")
plot(mod[3,], type = "l", main = "sigma^2")
plot(density(mod[3,(burn.in+1):iter]), main = "sigma^2")
