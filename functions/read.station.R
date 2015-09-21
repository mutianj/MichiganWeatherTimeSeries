##############################
## Function to read in data ##
read.station <- function(x){
  tdates <- scan("data/temperatures.dat",nlines = 1)
  ttemps <- scan("data/temperatures.dat",nlines = 1, skip = x)
  tloc <- scan("data/locations.dat",nlines = 1, skip = x-1)
  return(list("dates" = tdates, "temps" = ttemps, "coords" = tloc))
}
## Function to read in data ##
##############################
