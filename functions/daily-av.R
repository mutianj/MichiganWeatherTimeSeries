############################################
## function to extract seasonal component ##
## just calculates monthly averages       ##
daily.av <- function(x){
  dates <- as.POSIXlt(x[,1])
  dates$year <- 1900
  dates <- as.Date(dates)
  sn <- aggregate(x[,2], by = list(dates), FUN = "mean", na.rm = TRUE)
  for(i in 3:ncol(x)){
    sn <- cbind(sn,aggregate(x[,i], by = list(dates), FUN = "mean", na.rm = TRUE)[,2])
  }
  colnames(sn) <- c("dates",1:(ncol(x)-1))
  return(sn)
}
## function to extract seasonal component ##
############################################
