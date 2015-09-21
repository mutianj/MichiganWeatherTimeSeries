#################
## Trim Series ##
TrimNoData <- function(x){
  start <- head(which(is.na(dat$temp) == FALSE),1)
  end   <- tail(which(is.na(dat$temp) == FALSE),1)
  return(list("dates" = x$dates[start:end], "temps" = x$temps[start:end], "coords" = x$coords))
}
## Trim Series ##
#################
