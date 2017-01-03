heightweight <- read.fwf ("../doc/gelman/ARM_Data/earnings/wfw90.dat", widths=c(143,1,2,3))
height1 <- heightweight[,2]
height2 <- heightweight[,3]
weight <- heightweight[,4]
ok <- !is.na (height1+height2+weight) & weight<998 & height1<7 & height2<12
height <- 12*height1[ok] + height2[ok]
weight <- weight[ok]
n <- length(height)

C <- 200

censored <- weight>=C
weight.censored <- ifelse (censored,C,weight)

## Regression
c.height <- height - mean(height)

## Maximum likelihood in R
Loglik <- function (parameter.vector, x, y, C){
  a <- parameter.vector[1]
  b <- parameter.vector[2]
  sigma <- parameter.vector[3]
  ll.vec <- ifelse (y<C, dnorm (y, a + b*x, sigma, log=TRUE),
    pnorm ((a + b*x - C)/sigma, log=TRUE))
  return (sum (ll.vec))
}

 # optimize
inits <- runif (3)
mle <- optim (inits, Loglik,  lower=c(-Inf,-Inf,1.e-5), method="L-BFGS-B", control=list(fnscale=-1), x=c.height, y=weight.censored, C=200)
round (mle$par, 2)
