###########################
## Function to plot data ##
plot.station <- function(x, main = ""){
  plot(as.POSIXct(x = x$dates, origin = "1970-01-01 00:00:00", tz = "EST"), x$temps,  type = "l", xlab = "Date", ylab = "Temperature", main = main)
}
add.holes <- function(x){
  for(i in length(x$dates)){
    abline(v = as.POSIXct(x = x$dates, origin = "1970-01-01 00:00:00", tz = "EST"),
           col = "red",lty = 1)
  }
}

## Function to plot data ##
###########################
