# simulations for speed-of-light data in chapter 7

y <- scan ("lightspeed.dat", skip=4)

# plot the data

hist (y)

# fit the normal model (i.e., regression with no predictors)

lm.light <- lm (y ~ 1)
display (lm.light)
n <- length (y)

# create the replicated data

n.sims <- 1000
sim.light <- sim (lm.light, n.sims)

y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,] <- rnorm (1, sim.light$beta[s], sim.light$sigma[s])
}

# plot the replicated data

par (mfrow=c(5,4))
for (s in 1:20){
  hist (y.rep[s,])}

# perform the numerical test

test <- function (y){
  min (y)
}

test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- test (y.rep[s,])
}

# plot the histogram of test statistics of replications and of actual data

hist (test.rep, xlim=range (test(y), test.rep))
lines (rep (test(y), 2), c(0,n))
