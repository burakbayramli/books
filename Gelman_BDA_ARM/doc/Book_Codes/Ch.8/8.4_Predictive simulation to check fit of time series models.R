## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/unemployment
library ("arm")

unemployment <- read.table ("unemployment.dat", header=TRUE)
year <- unemployment$year
y <- unemployment$unemployed.pct

## Plot of the unemployment rate

par (mar=c(4,4,2,2))
plot (year, y, type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.2, cex.lab=1.2)
axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.2)

## Fitting a 1st-order autogregression

n <- length (y)
y.lag <- c (NA, y[1:(n-1)])
lm.lag <- lm (y ~ y.lag)
display (lm.lag)

## Simulating replicated datasets 

b.hat <- coef (lm.lag)        # vector of 2 regression coefs
s.hat <- sigma.hat (lm.lag)   # residual sd

n.sims <- 1000
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% b.hat
    y.rep[s,t] <- rnorm (1, prediction, s.hat)
  }
}

## Including uncertainty in the estimated parameters

lm.lag.sim <- sim (lm.lag, n.sims)       # simulations of beta and sigma
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <-  c (1, y.rep[s,t-1]) %*% lm.lag.sim$coef[s,]
    y.rep[s,t] <- rnorm (1, prediction, lm.lag.sim$sigma[s])
  }
}

## Plot of simulated unemployment rate series

par (mfrow=c(5,3), mar=c(4,4,2,2))
for (s in 1:15){
  plot (year, y.rep[s,], type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0),
        main=paste("simulation #", s, sep=""), cex.main=0.95)
  axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0))
}

## Numerical model check

Test <- function (y){
  n <- length (y)
  y.lag <- c (NA, y[1:(n-1)])
  y.lag2 <- c (NA, NA, y[1:(n-2)])
  sum (sign(y-y.lag) != sign(y.lag-y.lag2), na.rm=TRUE)
}

n.sims <- 1000
print (Test (y))
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

print (mean (test.rep > Test(y)))
print (quantile (test.rep, c(.05,.5,.95)))
