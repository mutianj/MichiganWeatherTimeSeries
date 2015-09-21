####################################################
## function to get MA ratios like the one in ITSM ##
getMAratios <- function(mod){mod$theta/(1.96*mod$se.theta)}
