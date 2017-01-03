library(boot)
# Fit a piecewise linear model where the slope is 0 after the change point
# The model is very simple: lm( y ~ pmin(x, cp)) where cp is the change point. This
# gives standard regression up to the change point, and then no change afer the change point, i.e. a slope of 0.
# Of course, you need to iterate to find the actual change point.
#
piecewise.linear.simple0<- function (x, y, middle = 1) 
  #  Fits a piecewise linear model where the slope is 0 after the change point
{
  piecewise.linear.likelihood <- function(alpha, x, y) {
    N <- length(x)
    fit <- lm(y ~ pmin(x,alpha) )
    Beta <- coefficients(fit)
    Mu <- Beta[1] + Beta[2] * pmin(x,alpha) 
    SSE <- sum(fit$residuals^2)
    sigma2 <- SSE/N
    likelihood <- sum(dnorm(y, mean = Mu, sd = sqrt(sigma2), log=TRUE))
    #    browser()
    return(likelihood)
  }
  r <- range(x)
  offset <- r * (1 - middle)/2
  low <- min(x) + offset
  high <- max(x) - offset
  temp <- optimize(piecewise.linear.likelihood, c(low, high), 
                   x = x, y = y, maximum = TRUE)
  return(temp$maximum)
}

piecewise.linear0 <- function (x, y, middle = 1, CI = FALSE, bootstrap.samples = 1000, 
                               sig.level = 0.05) {
  # Fit a piecewise linear function, but where the slope is zero after the change point
  alpha <- piecewise.linear.simple0(x, y, middle)
  model <- lm(y ~ pmin(x,alpha))
  out <- NULL
  out$change.point <- alpha
  out$model <- model
  out$x <- seq(min(x), max(x), length = 200)
  out$y <- predict(out$model, data.frame(x = out$x))
  out$CI <- CI
  class(out) <- "PiecewiseLinear"
  if (CI == FALSE) {return(out)} else {
    #browser()
    data <- data.frame(x = x, y = y)
    my.cp <- function(data, index) {
      x <- data[index, 1]
      y <- data[index, 2]
      cp <- piecewise.linear.simple0(x, y)
      model <- lm(y ~ pmin(x,cp) )
      out <- c(cp, model$coefficients[2] )
      return(out)
    }
    boot.result <- boot(data, my.cp, R = bootstrap.samples)
    out$intervals <- apply(boot.result$t, 2, quantile, probs = c(sig.level/2, 
                                                                 1 - sig.level/2))
    colnames(out$intervals) <- c("Change.Point", "Initial.Slope")
    out$CI <- t(out$CI)
    return(out)
  }
}


