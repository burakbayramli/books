## Read & clean the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings
library ("arm")

heightweight <- read.fwf ("wfw90.dat", widths=c(143,1,2,3))
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

## Figure 18.8 (a)
par (mar=c(4.7,3,1,1))
hist (weight.censored+2.5, breaks=seq(82.5,205,5), xlab="weight measurement",
      mgp=c(1.8,.5,0), xaxt="n", yaxt="n", main="")
axis (1, seq(100,200,50), mgp=c(1.8,.5,0))
axis (2, seq(0,200,100), mgp=c(1.8,.5,0))

## Figure 18.8 (b)

par (mar=c(4.7,3,1,1))
plot (height+runif(n,-.15,.15), weight.censored+runif(n,-1.5,1.5), pch=20, xlab="height", ylab="weight measurement",
      mgp=c(1.8,.5,0), xaxt="n", yaxt="n", main="", cex=.4)
axis (1, seq(60,80,10), mgp=c(1.8,.5,0))
axis (2, seq(100,200,50), mgp=c(1.8,.5,0))

## Regression
c.height <- height - mean(height)
censored.0 <- lm (weight ~ c.height)
display (censored.0)

## Censoring 
censored.1 <- lm (weight.censored ~ c.height, subset=!censored)
display (censored.1)

## Naive regression imputing the censoring points
censored.2 <- lm (weight.censored ~ c.height)
display (censored.2)

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

z <- ifelse (weight.censored==C, NA, weight.censored)
data <- list (x=c.height, y=weight.censored, z=z, n=n, C=C)
inits <- function(){
  list (a=rnorm(1), b=rnorm(1), sigma.y=runif(1))}
params <- c ("a", "b", "sigma.y")

censoring.1 <- bugs (data, inits, params, "censoring.bug", n.iter=10, debug=TRUE)
print(censoring.1)

## Gibbs sampler
 # starting values
n.censored <- sum (censored)
z[censored] <- runif (n.censored, C, 2*C)

 # regression if exact values were known
x <- c.height
lm.1 <- lm (z ~ x)
sim.1 <- sim (lm.1, n.sims=1)
a <- sim.1$coef[1]
b <- sim.1$coef[2]
sigma <- sim.1$sigma

 # impute missing values given fitted regression
rnorm.trunc <- function (n, mu, sigma, lo=-Inf, hi=Inf){
  p.lo <- pnorm (lo, mu, sigma)
  p.hi <- pnorm (hi, mu, sigma)
  u <- runif (n, p.lo, p.hi)
  return (qnorm (u, mu, sigma))
}

 # sample missing z_i's
z[censored] <- rnorm.trunc (n.censored, a + b*x[censored], sigma, lo=C)

 # puting it together in a loop
n.chains <- 3
n.iter <- 100
sims <- array (NA, c(n.iter, n.chains, 3 + n.censored))
dimnames (sims) <- list (NULL, NULL,
  c ("a", "b", "sigma", paste ("z[", (1:n)[censored], "]", sep="")))
for (m in 1:n.chains){
  z[censored] <- runif (n.censored, C, 2*C)
  for (t in 1:n.iter){
    lm.1 <- lm (z ~ x)
    sim.1 <- sim (lm.1, n.sims=1)
    a <- sim.1$coef[1]
    b <- sim.1$coef[2]
    sigma <- sim.1$sigma
    z[censored] <- rnorm.trunc (n.censored, a + b*x[censored], sigma, lo=C)
    sims[t,m,] <- c (a, b, sigma, z[censored])
  }
}

sims.bugs <- as.bugs.array (sims)
print (sims.bugs)
plot (sims.bugs)

