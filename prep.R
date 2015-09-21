rm(list=ls())
###############
## libraries ##
## I wrote a bunch of functions to read in
## and manipulate the data easier
## the code for all of them are in the 'functions' folder
## most of them are minor edits to functions in itsmr
library(itsmr)
source("functions/sourceDir.R")
sourceDir("functions")

#################################################################
## import csvs and convert them to one file 'temperatures.dat' ##
files <- list.files("data/csv", full.names = TRUE)

ids <- matrix(nrow = length(files),ncol = 3)
for(i in 1:length(files)){
  ids[i,] <- as.numeric(strsplit(scan(files[i], what = character(),nlines=1,
                                      skip = 1, quiet = TRUE), ",")[[1]][1:3])
}

ids.mast <- unique(ids[,1:2])
ids.locs <- matrix(nrow = 0, ncol = 2)
big.dat  <- matrix(seq(as.POSIXct("1990-01-01 00:00:00", tz = "GMT"),
                       as.POSIXct("2012-12-31 23:00:00", tz = "GMT"), by="hour"), nrow = 1)

write.table(big.dat, "data/temperatures.dat", col.names = FALSE, row.names = FALSE)
write.table(ids.locs, "data/locations.dat", col.names = FALSE, row.names = FALSE)

for(j in 1:nrow(ids.mast)){
  dat <- matrix(nrow = 0, ncol = 5)
  
  for(i in which(ids[,1] == ids.mast[j,1] & ids[,2] == ids.mast[j,2])){
    tmp <- read.csv(files[i])
    ids.locs <- unlist(c(tmp[1,c("LONG","LAT")]))
    tmp <- tmp[which(tmp$TEMP != 999.9),c("YR","M","D","HR","TEMP")]
    dat <- rbind(dat,tmp)
  }
  dat <- cbind(dat[,5], apply(dat, 1, ConvertTime))
  big.dat <- rbind(big.dat,NA)
  big.dat[2,(dat[,2]-big.dat[1,1])/3600 + 1] <- dat[,1]

  write.table(t(big.dat[2,]), "data/temperatures.dat", append = TRUE, col.names = FALSE, row.names = FALSE)
  write.table(t(ids.locs), "data/locations.dat", append = TRUE, col.names = FALSE, row.names = FALSE)
  big.dat <- matrix(big.dat[-2,], nrow = 1)
  print(j)
}

################################################
## Generate time series plot for each station ##
fig.num <- cbind(seq(1,181, by = 8),seq(8,181, by = 8))
fig.num[23,2] <- 181

for(i in 1:nrow(fig.num)){
  postscript(paste("figures/hourly/hourly_timeseries_",i,".eps", sep = ""), width=14, height=6,
             horizontal = FALSE, onefile = FALSE, paper = "special",
             family = "ComputerModern", encoding = "TeXtext.enc")##, pointsize = 16)
  par(mfrow = c(4,2), oma = c(0,0,0,0), mar = c(2,2,1,1))
  for(j in fig.num[i,1]:fig.num[i,2]){
    dat <- read.station(j)
    holes <- FindMissing(dat)
    if(length(dat$temps) != length(holes$temps)){
      plot.station(dat, paste("Station",j))
    }else{
      plot(as.POSIXct(x = dat$dates, origin = "1970-01-01 00:00:00", tz = "EST"),
           rep(0, length(dat$dates)),  type = "n", xlab = "Date", ylab = "Temperature",
           main = paste("Station",j))
      text(as.POSIXct(x = median(dat$dates), origin = "1970-01-01 00:00:00", tz = "EST"),
           0, "No Data")
    }
  }
  dev.off()
}

######################################################
## generate pdf containing hourly time series plots ##
system("R CMD latex latex/hourly_timeseries.tex")
system("dvipdfmx hourly_timeseries.dvi")
system("rm hourly_timeseries.dvi")
system("rm hourly_timeseries.log")
system("rm hourly_timeseries.aux")
system("mv hourly_timeseries.pdf docs/hourly_timeseries.pdf")

####################################################################################
## convert hourly to daily data ####################################################
dat <- read.station(1)

day <- daily(as.POSIXct(x = dat$dates, origin = "1970-01-01 00:00:00", tz = "EST"))

for(i in 1:181){
  dat <- read.station(i)
  daily.avg <- aggregate(dat$temps, by = list(day), FUN = "mean", na.rm = TRUE)
  write.table(daily.avg, paste("data/daily/station-",i,".dat",sep = ""),
              row.names = FALSE, col.names = FALSE)
}
## convert hourly to daily data ####################################################
####################################################################################

####################################################################################
## select stations to use for analysis #############################################

######################################################
## select stations                                  ##
## I did this by looking at each time series plot   ##
## and selecting the stations that had data between ##
## 2007 and now.                                    ##
select <- c(4,6,8,10,12,14,15,21,23,25,27,29,33,34,36,38,39,41,43,
            45,47,50,52,54,55,57,58,61,63,65,67,69,71,73,75,77,79,
            81,83,85,89,90,93,95,96,99,100,104,105,106,108,111,112,
            114,116,120,124,126,127,131,133,137,139,141,143,146,147,
            158,161,162,163,164,165,166,168,169,173,174,176,177,178,180)

###############################
## read in selected stations ##
dat <- read.table(paste("data/daily/station-",4,".dat", sep = ""))
for(i in select[-1]){
  dat <- cbind(dat,read.table(paste("data/daily/station-",i,".dat", sep = ""))[,2])
}

###################################################
## find common starting date for all time series ##
check.start <- c()
for(i in 2:83){check.start[i] <- head(which(is.na(dat[,i]) == FALSE),1)}
dat <- dat[max(check.start, na.rm = TRUE):nrow(dat),] ## select January 5, 2007 as time one.

######################
## remove leap year ##
dat <- dat[-which(dat[,1] == '2012-02-29'),]
dat <- dat[-which(dat[,1] == '2008-02-29'),]

##################################
## Select holdout stations      ##
## base on missing observations ##
## holout 10 stations           ##
locs <- read.table("data/locations.dat", header = FALSE)[select,]

na.count <- NA
for(i in 2:83){
  na.count[i] <- length(which(is.na(dat[,i]) == TRUE))
}

hold.stations <- c(4,40,which(na.count >= 38))

write.table(dat[,c(1,hold.stations)],"data/select-stations-holdout.dat", row.names = FALSE, col.names = FALSE)
write.table(locs[hold.stations-1,],  "data/select-locations-holdout.dat", row.names = FALSE, col.names = FALSE)

#########################
## Select holdout time ##
## holout 2012 data    ##
hold.time <- 1826:nrow(dat)

write.table(dat[hold.time,-(hold.stations-1)],"data/select-time-holdout.dat", row.names = FALSE, col.names = FALSE)

################################
## write out selected dataset ##
write.table(dat[-hold.time,-hold.stations],"data/select-stations.dat", row.names = FALSE, col.names = FALSE)

#######################################################
## write out selected dataset's coordinate locations ##
write.table(locs[-c(hold.stations-1),], "data/select-locations.dat", row.names = FALSE, col.names = FALSE)
## select stations to use for analysis #############################################
####################################################################################
