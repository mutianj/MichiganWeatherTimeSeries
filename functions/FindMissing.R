#########################
## Find missing values ##
FindMissing <- function(x){
  idx <- which(is.na(x$temps) == TRUE)
  return(list("dates" = x$dates[idx], "temps" = x$temps[idx], "coords" = x$coords))
}
## Find missing values ##
#########################
