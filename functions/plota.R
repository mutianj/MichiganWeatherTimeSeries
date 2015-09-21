########################################
## modified plota function from itsmr ##
plota <- function(u, v = NULL, h = 40, acf.main = "ACF", pacf.main = "PACF"){
  plot.data.acf = function(x) {
    n = length(x)
    h = min(h, n - 1)
    op = par(mfrow = c(1, 2))
    acf = acf(x, lag.max = h, type = "correlation", plot = FALSE)$acf
    plot(0:h, acf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = acf.main, col = "blue", bty = "n")
    b = 1.96/sqrt(n)
    abline(h = b, lty = 2)
    abline(h = -b, lty = 2)
    abline(h = 0)
    legend("topright", "Data", fill = "blue", bty = "n")
    pacf = acf(x, lag.max = h, type = "partial", plot = FALSE)$acf
    pacf = c(1, pacf)
    plot(0:h, pacf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = pacf.main, col = "blue", bty = "n")
    abline(h = b, lty = 2)
    abline(h = -b, lty = 2)
    abline(h = 0)
    legend("topright", "Data", fill = "blue", bty = "n")
    par(op)
  }
  plot.model.acf = function(a) {
    op = par(mfrow = c(1, 2))
    acf = ARMAacf(ar = a$phi, ma = a$theta, lag.max = h)
    pacf = ARMAacf(ar = a$phi, ma = a$theta, lag.max = h, 
      pacf = TRUE)
    pacf = c(1, pacf)
    plot(0:h, acf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = acf.main, col = "red", bty = "n")
    abline(h = 0)
    legend("topright", "Model", fill = "red")
    plot(0:h, pacf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = pacf.main, col = "red", bty = "n")
    abline(h = 0)
    legend("topright", "Model", fill = "red", bty = "n")
    par(op)
  }
  plot.combo.acf = function(x, a) {
    n = length(x)
    h = min(h, n - 1)
    op = par(mfrow = c(1, 2))
    acf = acf(x, lag.max = h, type = "correlation", plot = FALSE)$acf
    plot(0:h, acf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = acf.main, col = "blue", bty = "n")
    acf = ARMAacf(ar = a$phi, ma = a$theta, lag.max = h)
    for (i in 0:h) lines(c(i + 0.2, i + 0.2), c(0, acf[i + 1]), col = "red")
    b = 1.96/sqrt(n)
    abline(h = b, lty = 2)
    abline(h = -b, lty = 2)
    abline(h = 0)
    legend("topright", c("Data", "Model"), fill = c("blue", "red"), bty = "n")
    pacf = acf(x, lag.max = h, type = "partial", plot = FALSE)$acf
    pacf = c(1, pacf)
    plot(0:h, pacf, ylim = c(-1, 1), type = "h", xlab = "Lag", 
         ylab = "", main = pacf.main, col = "blue", bty = "n")
    pacf = ARMAacf(ar = a$phi, ma = a$theta, lag.max = h, 
      pacf = TRUE)
    pacf = c(1, pacf)
    for (i in 0:h) lines(c(i + 0.2, i + 0.2), c(0, pacf[i + 1]), col = "red")
    abline(h = b, lty = 2)
    abline(h = -b, lty = 2)
    abline(h = 0)
    legend("topright", c("Data", "Model"), fill = c("blue", "red"), bty = "n")
    par(op)
  }
  if (is.null(v)) 
    if (is.list(u)) 
      plot.model.acf(u)
    else plot.data.acf(u)
  else if (is.list(u)) 
    plot.combo.acf(v, u)
  else plot.combo.acf(u, v)
}
