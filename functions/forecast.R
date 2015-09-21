###########################################
## modified forecast function from itsmr ##
forecast <- function(x, xv, a, h = 10, opt = 2, mn = 0, xtab = 0, main = NULL){
  f = .forecast.transform(x, xv, a, h, 1)
  f$pred = f$pred + mn
  if (!is.null(f$phi)) {
    psi = ma.inf(list(phi = f$phi, theta = a$theta), h)
    g = function(j) sum(psi[1:j]^2)
    se = sqrt(a$sigma2 * sapply(1:h, g))
    l = f$pred - 1.96 * se
    u = f$pred + 1.96 * se
    f = list(pred = f$pred, se = se, l = l, u = u)
  }
  if (opt > 0) {
    if (is.null(f$se)) 
      cat(" Step     Prediction    Lower Bound    Upper Bound\n")
    else cat(" Step     Prediction      sqrt(MSE)    Lower Bound    Upper Bound\n")
    for (i in 1:h) {
      cat(format(i, width = 5))
      cat(format(f$pred[i], width = 15))
      if (!is.null(f$se)) 
        cat(format(f$se[i], width = 15))
      cat(format(f$l[i], width = 15))
      cat(format(f$u[i], width = 15))
      cat("\n")
    }
  }
  if (xtab == 1){
    tab <- cbind(1:h,f$pred,se,l,u)
    colnames(tab) <- c("Step","Prediction","$\\sqrt{MSE}$",
                       "Lower Bound", "Upper Bound")
    tab <- xtable(tab, digits = c(0,0,4,4,4,4))
    print(tab, table.placement = "!h",include.rownames=FALSE,
      sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE))
  }
  if (opt > 1)
    x = x + mn
    plot.forecast(x, f, main)
  return(invisible(f))
}


plot.forecast <- function(x, f, main){
  x <- x
  n = length(x)
  m = length(f$pred)
  ymin = min(x, f$l)
  ymax = max(x, f$u)
  if (is.ts(x)) {
    u = time(x)
    v = (0:(n + m - 1))/frequency(x) + start(x)
  }
  else {
    u = 1:n
    v = 1:(n + m)
  }
  plot.default(u, x, xlim = c(v[1], v[n + m]), ylim = c(ymin, ymax),
               xlab = "", ylab = "", type = "o", col = "blue", bty = "n",
               pch = 19, cex = .5, xaxt = "n", main = main)
  axis(1, at = c(seq(0,n,10),n + m))
  lines(v[(n + 1):(n + m)], f$pred[1:m], type = "o", col = "red")
  lines(v[(n + 1):(n + m)], f$l[1:m], type = "l", col = "red", 
        lty = 2)
  lines(v[(n + 1):(n + m)], f$u[1:m], type = "l", col = "red", 
        lty = 2)
}
