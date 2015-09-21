##################################################
## function to create daily aggregator variable ##
daily <- function(x){
  x <- as.POSIXlt(x)
  x$hour <- 1
  as.Date(x)
}
##################################################
