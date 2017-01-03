## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.2_Partial pooling with no predictors.R") # where data was cleaned
# close the Bugs window to proceed

## Call Bugs from R 

# Note: the Bugs code on pages 353-355 from this section are on #
# the file radon.1.bug and  will be called in the code below    #

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

radon.bugs.1 <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug",
    n.chains=3, n.iter=500,  bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE)

# Plot Figure 16.2
radon.1.noburnin <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", n.chains=3, n.iter=500, n.burnin=0)
par (mar=c(5,5,3,2)+.1)
plot (c(0,200), range(radon.1.noburnin$sims.array[6:208,,"mu.a"]), xlab="iteration",
      ylab="", xaxs="i", cex.lab=1.2, cex.axis=1.1, type="n")
mtext (expression(mu[alpha]), 2, 3, cex=1.1) 
for (j in 1:3){
  lines (1:203, radon.1.noburnin$sims.array[6:208,j,"mu.a"], lwd=.5)
}

## Accessing the simulations
attach.bugs (radon.bugs.1)

 # 90% CI for beta
quantile (b, c(0.05, 0.95))

 # Prob. avg radon levels are higher in county 36 than in county 26
mean (a[,36] > a[,26])

## Fitted values, residuals and other calculations
attach.bugs (radon.bugs.1)
a.multilevel <- rep (NA, J)
for (j in 1:J){
  a.multilevel[j] <- median (a[,j])}
b.multilevel <- median (b)

y.hat <- a.multilevel[county] + b.multilevel*x
y.resid <- y - y.hat

plot (y.hat, y.resid)

 # numeric calculations
n.sims <- 100
lqp.radon <- rep (NA, n.sims)
hennepin.radon <- rep (NA, n.sims)
for (s in 1:n.sims){
  lqp.radon[s] <- exp (rnorm (1, a[s,36] + b[s], sigma.y[s]))
  hennepin.radon[s] <- exp (rnorm (1, a[s,26] + b[s], sigma.y[s]))
}
radon.diff <- lqp.radon - hennepin.radon
hist (radon.diff)
print (mean (radon.diff))
print (sd (radon.diff))


