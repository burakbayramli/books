## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/lightspeed
library ("arm")

y <- scan ("lightspeed.dat", skip=4)

## Model fit 

light <- lm (y ~ 1)
display (light)

## Create the replicated data 

n.sims <- 1000
sim.light <- sim (light, n.sims)

## Create fake data 

n <- length (y)
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,] <- rnorm (n, sim.light$coef[s], sim.light$sigma[s])
}

## Histogram of replicated data (Figure 8.4)

par (mfrow=c(5,4), mar=c(3,1,2,1))
for (s in 1:20){
  hist (y.rep[s,], xlab="", ylab="", cex.main="1", yaxt="n", xlim=range(y))
}

## Write a function to make histograms with specified bin widths and ranges

Hist.preset <- function (a, width, ...){
  a.hi <- max (a, na.rm=TRUE)
  a.lo <- min (a, na.rm=TRUE)
  if (is.null(width)) width <- min (sqrt(a.hi-a.lo), 1e-5)
  bin.hi <- width*ceiling(a.hi/width)
  bin.lo <- width*floor(a.lo/width)
  hist (a, breaks=seq(bin.lo,bin.hi,width), ...)
}

## Run the function

par (mfrow=c(5,4), mar=c(3,1,2,1))
for (s in 1:20){
  Hist.preset (y.rep[s,], width=5, xlab="", ylab="", cex.main="1", yaxt="n",
               xlim=range(y), main=paste("Replication #",s,sep=""))
}

## Numerical test

Test <- function (y){
  min (y)
}
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

## Histogram Figure 8.5

par (mfrow=c(1,1))
hist (test.rep, xlim=range (Test(y), test.rep), yaxt="n", ylab="",
 xlab="", main="Observed T(y) and distribution of T(y.rep)")
lines (rep (Test(y), 2), c(0,10*n))

##############################################################################
## Read the cleaned data
# All data are at http://www.stat.columbia.edu/~gelman/arm/examples/roaches
library ("arm")

roachdata <- read.csv ("roachdata.csv")
attach.all (roachdata)

## Fit the model

glm.1 <- glm (y ~ roach1 + treatment + senior, family=poisson,
  offset=log(exposure2))
display (glm.1)

## Comparing the data to a replicated dataset

n <- length(y)
X <- cbind (rep(1,n), roach1, treatment, senior)
y.hat <- exposure2 * exp (X %*% coef(glm.1))
y.rep <- rpois (n, y.hat)

print (mean (y==0))
print (mean (y.rep==0))

## Comparing the data to 1000 replicated datasets

n.sims <- 1000
sim.1 <- sim (glm.1, n.sims)
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.hat <- exposure2 * exp (X %*% sim.1$coef[s,])
  y.rep[s,] <- rpois (n, y.hat)
}

 # test statistic 

Test <- function (y){
  mean (y==0)
}
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

# p-value
print (mean (test.rep > Test(y)))

## Checking the overdispersed model

glm.2 <- glm (y ~ roach1 + treatment + senior, family=quasipoisson,
  offset=log(exposure2))
display (glm.2)

 # 1000 replicated datasets

n.sims <- 1000
sim.2 <- sim (glm.2, n.sims)
y.rep <- array (NA, c(n.sims, n))
overdisp <- summary(glm.2)$dispersion
for (s in 1:n.sims){
  y.hat <- exposure2 * exp (X %*% sim.2$coef[s,])
  a <- y.hat/(overdisp-1)              # using R's parametrization for the
  y.rep[s,] <- rnegbin (n, y.hat, a)   # negative binomial distribution
}

test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

# p-value
print (mean (test.rep > Test(y)))
