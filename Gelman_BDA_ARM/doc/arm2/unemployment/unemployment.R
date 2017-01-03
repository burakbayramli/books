# Time series fit and model checking for unemployment series

unemployment <- read.table ("unemployment.dat", header=TRUE)
year <- unemployment$year
y <- unemployment$unemployed.pct

# plot the unemployment rate

postscript ("c:/books/multilevel/unemployment1.ps", height=3, width=4)
par (mar=c(4,4,2,2))
plot (year, y, type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.2, cex.lab=1.2)
axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.2)
dev.off()

# fit a 1st-order autogregression

n <- length (y)
print (n)
y.lag <- c (NA, y[1:(n-1)])
lm.lag <- lm (y ~ y.lag)
display (lm.lag)

# simulate replicated datasets (using beta.hat, sigma.hat)

b.hat <- beta.hat (lm.lag)
s.hat <- sigma.hat (lm.lag)

n.sims <- 1000
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% b.hat
    y.rep[s,t] <- rnorm (1, prediction, s.hat)
  }
}

# simulate replicated datasets (full uncertainty)

lm.lag.sim <- sim (lm.lag, n.sims)
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <-  c (1, y.rep[s,t-1]) %*% lm.lag.sim$beta[s,]
    y.rep[s,t] <- rnorm (1, prediction, lm.lag.sim$sigma[s])
  }
}

# plot the simulated unemployment rate series

postscript ("c:/books/multilevel/unemployment2.ps", height=5, width=10)
par (mfrow=c(3,5), mar=c(4,4,2,2))
for (s in 1:15){
  plot (year, y.rep[s,], type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.5, cex.lab=1.5, main=paste ("simulation", s), cex.main=1.5)
  axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.5)
}
dev.off()

# numerical model check

test <- function (y){
  n <- length (y)
  y.lag <- c (NA, y[1:(n-1)])
  y.lag2 <- c (NA, NA, y[1:(n-2)])
  sum (sign(y-y.lag) != sign(y.lag-y.lag2), na.rm=TRUE)
}

print (test (y))

test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- test (y.rep[s,])
}

print (mean (test.rep > test(y)))
print (quantile (test.rep, c(.05,.5,.95)))
