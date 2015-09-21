########################################
## modified plotc function from itsmr ##
plotc <- function(y1, y2 = NULL, main = NULL){
  if (is.null(y2)) {
    if (is.ts(y1)) 
      x1 = time(y1)
    else x1 = 1:length(y1)
    plot.default(x1, y1, xlab = "", ylab = "", main = main,
                 type = "o", pch = 19, cex = .5, col = "blue", bty = "n")
    return(invisible(NULL))
  }
  n1 = length(y1)
  n2 = length(y2)
  if (is.ts(y1)) {
    x1 = time(y1)
    x2 = (0:(n2 - 1))/frequency(y1) + start(y1)
  }
  else {
    x1 = seq(1, n1)
    x2 = seq(1, n2)
  }
  xmax = max(x1, x2)
  xmin = min(x1, x2)
  ymax = max(y1, y2)
  ymin = min(y1, y2)
  plot.default(x1, y1, xlim = c(xmin, xmax), main = main,
               ylim = c(ymin, ymax), xlab = "", ylab = "",
               type = "o", pch = 19, cex = .5, col = "blue", bty = "n")
  lines(x2, y2, type = "l", col = "red")
}
