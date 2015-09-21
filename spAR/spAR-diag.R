mod <- as.matrix(read.table("mod"))
burn.in <- 100
iter <- 1000

est.pars <- rowMeans(mod[,-c(1:burn.in)])

Y.1 <- read.table("data/residual.dat")[,2]
Y.1[is.na(Y.1)] <- 0


F.1 <- c(NA,NA)
for(i in 3:length(Y.1)){
  F.1[i] <- Y.1[i-1]*est.pars[1] + Y.1[i-2]*est.pars[2]
}

R.1 <- Y.1-F.1

par(ask=T)

plot(Y.1, type = "l");points(F.1, col = "red", type = "l")



par(mfrow = c(4,2))
plot(mod[1,], type = "l", main = "phi 1")
plot(density(mod[1,(burn.in+1):iter]), main = "phi 1")
plot(mod[2,], type = "l", main = "phi 2")
plot(density(mod[2,(burn.in+1):iter]), main = "phi 2")
plot(mod[3,], type = "l", main = "sigma^2")
plot(density(mod[3,(burn.in+1):iter]), main = "sigma^2")
pacf(Y.1, main = "Before", ylim = c(-.8,.8))
pacf(R.1[-c(1,2)], main = "After", ylim = c(-.8,.8))
