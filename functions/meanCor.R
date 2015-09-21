################################
## mean correct a time series ##
meanCor <- function(series){series-mean(series, na.rm = TRUE)}
