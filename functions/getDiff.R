########################################
## calculate differences of any order ##
getDiff <- function(series, order = 1){
  for(j in 1:order){
    diff.series <- c()
    for(i in 2:length(series)){
      diff.series[i-1] <- series[i]-series[i-1]
    }
    series <- diff.series
  }
  return(series)
}
