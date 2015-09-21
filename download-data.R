rm(list=ls())
#################################
## download master staion list ##
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
repeat {
  try(download.file(file, "data/ish-history.csv", quiet=TRUE))
  if (file.info("data/ish-history.csv")$size > 0) {break}
}
st <- read.csv("data/ish-history.csv")
names(st)[c(3,10)] <- c("NAME", "ELEV")
st <- st[,-5]
st <- st[st$CTRY == "US",]

######################
## edit master list ##
st$LAT   <- st$LAT/1000
st$LON   <- st$LON/1000
st$ELEV  <- st$ELEV/10
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END   <- as.numeric(substr(st$END, 1, 4))

###################################
## select only michigan stations ##
mi.list <- st[st$STATE == "MI" &
              (st$BEGIN <= 2012 & st$END >= 1990 & !is.na(st$BEGIN)), ]

############################################################
## download all available hourly weather station data for ##
## michigan from 1990 to 2012                             ##
outputs <- as.data.frame(matrix(NA, dim(mi.list)[1], 2))
names(outputs) <- c("FILE", "STATUS")
## STATUS equal to 0 equals downloaded, 256 equals not downloaded
for (y in 1990:2012) {
  y.mi.list <- mi.list[mi.list$BEGIN <= y & mi.list$END >= y, ]
  for (s in 1:dim(y.mi.list)[1]) {
    outputs[s,1] <- paste(sprintf("%06d", y.mi.list[s,1]), "-",
                          sprintf("%05d", y.mi.list[s,2]), "-", y, ".gz", sep="")
    wget <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                  y, "/", outputs[s,1], sep="")
    outputs[s,2] <- try(system(wget, intern=FALSE, ignore.stderr=TRUE))
  }
}

######################
## Decompress Files ##
system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)
system("mv *.gz data/raw-gz",intern = FALSE, ignore.stderr = TRUE)

#############################################################
## import data, reformat it, and write the data out as csv ##
files <- list.files("data/raw")
column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1,4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)
stations <- as.data.frame(matrix(NA, length(files), 6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")
for (i in 1:length(files)) {
  dat <- read.fwf(paste("data/raw/", files[i], sep=""), column.widths)
  dat <- dat[,c(2:8,10:11,13,16,19,29,31,33)]
  names(dat) <- c("USAFID","WBAN","YR","M","D","HR","MIN","LAT","LONG",
                  "ELEV","WIND.DIR", "WIND.SPD", "TEMP","DEW.POINT","ATM.PRES")
  dat$LAT        <- dat$LAT/1000
  dat$LONG       <- dat$LONG/1000
  dat$WIND.SPD   <- dat$WIND.SPD/10
  dat$TEMP       <- dat$TEMP/10
  dat$DEW.POINT  <- dat$DEW.POINT/10
  dat$ATM.PRES   <- dat$ATM.PRES/10
  write.csv(dat, file=paste("data/csv/", files[i], ".csv", sep=""), row.names=FALSE)
  stations[i,1:3] <- dat[1,1:3]
  stations[i,4:6] <- dat[1,8:10]
}

#################################
## write out station list file ##
write.csv(stations, file="data/stations.csv", row.names=FALSE)


