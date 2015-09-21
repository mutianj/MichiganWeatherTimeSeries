rm(list=ls())
###############
## libraries ##
library(MASS)
library(nlme)

##########################################
## Generates AR 2 autocovariance matrix ##
genAR <- function(Phi, sig, n){
  RhoVec    <- c(1)
  RhoVec[2] <- Phi[1]/(1-Phi[2])
  for(i in 3:n){RhoVec[i] <- Phi[1]*RhoVec[i-1] + Phi[2]*RhoVec[i-2]}
  Rho <- Vec2Sym(RhoVec)
  gamma.0 <- 1/(1-Phi[1]*RhoVec[2] - Phi[2]*RhoVec[3])*sig
  ARcov <- gamma.0*Rho
  return(ARcov)
}
## Generates AR 2 autocovariance matrix ##
##########################################

############################################
## turns a vector into a symmetric matrix ##
## Used inside genAR function             ##
Vec2Sym <- function(Vec){
  n <- length(Vec)
  mat <- diag(1,n)
  for(i in 1:n){
    mat[i,i:n] <- Vec[1:(n-i+1)]
    mat[n-i+1,1:(n-i+1)] <- Vec[(n-i+1):1]
  }
  return(mat)
}
## turns a vector into a symmetric matrix ##
############################################

dat <- matrix(rep(0,5), ncol = 1)

corr <- corARMA(c(.8,-.2,.2), p = 3, q = 0, fixed = TRUE)
corr <- Initialize(corr, data = dat)
corr <- corMatrix(corr) 

gam.0 <- 1/(1-.8*.76 - -.2*.3333 - .2)*1
corr*gam0

genAR(c(.8,-.2),1,5)

gam0 <- function(sig, Phi, Rho){
  RhoP <- Rho[1:length(Phi),1:length(Phi)]
  sig/(1-t(Phi)%*%solve(RhoP)%*%Phi)
}

gam0(1,c(.8,-.2),corr)

tdat <- mvrnorm(1, rep(0, 400), corMatrix(corr))

par(mfrow = c(2,1));plot(tdat, type = "l");pacf(tdat)

lL <- function(Y, X, B, phi, sig)
  
  (-n/2)*(2*pi) -(1/2)*det(Sig) - (1/2)*t(Y-X%*%B)%*%solve(Sig)%*%(Y-X%*%B)
