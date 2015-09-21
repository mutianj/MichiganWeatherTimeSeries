rm(list=ls())

source("functions/daily-av.R")

#############################
## read in selected datset ##
dat <- read.table("data/select-stations.dat")

##################################################
## collect mean component from each time series ##
mn <- apply(dat[,2:73],2, mean, na.rm = TRUE)

###################################
## mean correct each time series ##
mn.long <- matrix(rep(mn,nrow(dat)), ncol = 72, byrow = TRUE)
dat.mn  <- cbind(dat[,1],dat[,2:73]-mn.long)

################################################################
## collect daily averages for each mean corrected time series ##
sn <- daily.av(dat.mn)
sn[,1] <- c(0:364)
sn <- as.matrix(sn)

########################################################
## extend sn to the length of the time series         ##
## so the seasonal component can easily be subtracted ##
sn.long <- matrix(NA,nrow = nrow(dat), ncol = ncol(dat))
for(i in 1:365){
  rows <- which(as.POSIXlt(dat[,1])$yday == sn[i,1])
  for(j in 1:length(rows)){
    sn.long[rows[j],] <- sn[i,]
  }
}

############################################################
## subtract mean and daily averages from each time series ##
res <- dat[,2:73] - sn.long[,2:73] - mn.long

######################################################################
## write out the mean trend, seasonal trend, and residual component ##
mean.trend <- data.frame(date = dat[,1], mn.long)
seasonal.trend <- data.frame(date = dat[,1], sn.long[,2:73])
residual <- data.frame(date = dat[,1], res)

## fix missing seasonal value ##
seasonal.trend[726,2:73] <- (seasonal.trend[725,2:73] + seasonal.trend[727,2:73])/2

## estimate missing data in timeseries with
## mean plus seasonal component
for(i in 2:73){
  rows <- which(is.na(dat[,i]) == TRUE)
  if(length(rows) > 0){
    dat[rows,i] <- seasonal.trend[rows,i] + mean.trend[rows,i]
  }
}

for(i in 2:73){
  residual[which(is.na(residual[,i]) == TRUE),i] <- 0
}

## write.table(mean.trend    , "data/mean.dat"           , row.names = FALSE, col.names = FALSE)
## write.table(seasonal.trend, "data/seasonal.dat"       , row.names = FALSE, col.names = FALSE)
## write.table(residual      , "data/residual.dat"       , row.names = FALSE, col.names = FALSE)

write.table(dat, "data/select-stations-filled.dat", row.names = FALSE, col.names = FALSE)
