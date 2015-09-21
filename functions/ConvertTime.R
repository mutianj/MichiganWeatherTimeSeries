################################
## Functions Used within Loop ##
ConvertTime <- function(datrow){
  as.numeric(as.POSIXct(paste(datrow[1],datrow[2],datrow[3],datrow[4], sep = ":"),
                        format = "%Y:%m:%d:%H", tz = "GMT"))
}
## Functions Used within Loop ##
################################
