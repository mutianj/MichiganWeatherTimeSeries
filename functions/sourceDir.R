###################################
## source a directory of R files ##
## taken straight from help(source)
sourceDir <- function(path, trace = TRUE, ...){
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")){
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
