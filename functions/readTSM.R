####################################################################
## function to read dat in from a .TSM file normally used in ITSM ##
readTSM <- function(x){
  check.Uni <- grepl("Univariate",readLines(x, n=1))
  check.Mul <- grepl("Multivariate",readLines(x, n=1))
  if(check.Uni == FALSE & check.Mul == FALSE){
    print("this is a Univariate one")
    tsm <- readLines(x)
    out <- c()
    for(i in 1:length(tsm)){
      out <- c(out, as.numeric(unlist(strsplit(tsm[i], " "))[1]))
    }
  }else if(check.Uni == TRUE){
    print("this is a Univariate one")
    tsm <- readLines(x)
    sta <- which(grepl("Series",tsm) == TRUE)
    fin <- which(grepl("Regression components", tsm) == TRUE)
    tsm <- as.matrix(read.table(x, skip = sta+1, nrows = fin-sta-2,
                                sep = "\t", strip.white = TRUE))
    out <- c()
    for(i in 1:nrow(tsm)){
      t1 <- unlist(strsplit(tsm[i,1], " "))
      out <- c(out, as.numeric(t1[which(t1 != "")]))
    }    
  }else if(check.Mul == TRUE){
    print("this is a Multivariate one")
    out <- NULL
  }
  return(out)
}
####################################################################

## example ##
## sunspots <- readTSM("SUNSPOTS.TSM")
