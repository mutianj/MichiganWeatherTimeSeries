##############################################
## function to get AR ratios like ITSM does ##
getARratios <- function(mod){mod$phi/(1.96*mod$se.phi)}
