# Code examples for the simulation chapter of ADAfaEPoV
# See that chapter for context


##### Illustration of the rejection method

# Draw one sample by the rejection method from the target distribution
# Inputs: target probability density function (dtarget)
  # proposal p.d.f. (dproposal)
  # random number generator for proposal (rproposal)
  # scaling factor (M)
# Presumes: dtarget takes a number, and nothing else, and returns a number;
  # so does dproposal;
  # rproposal takes the number of variables desired, and nothing else
# Output: A single random number, drawn from the target distribution
rrejection.1 <- function(dtarget,dproposal,rproposal,M) {
  rejected <- TRUE
  while(rejected) {
    R <- rproposal(1)
    U <- runif(1)
    rejected <- (M*U*dproposal(R) < dtarget(R))
  }
  return(R)
}

# Draw multiple samples by the rejection method
  # Call rrejection.1() multiple times, using replicate()
# Inputs: number of samples to draw (n)
  # target probability density function (dtarget)
  # proposal p.d.f. (dproposal)
  # random number generator for proposal (rproposal)
  # scaling factor (M)
# Calls: rrejection.1
# Output: vector of numbers of length n
rrejection <- function(n,dtarget,dproposal,rproposal,M) {
  replicate(n,rrejection.1(dtarget,dproposal,rproposal,M))
}

# Comment on these functions:
# This is not "industrial strength" programming, because the arguments which
# are functions (dtarget, dproposal, rproposal) are only allowed to be
# functions of one argument.  If we wanted to draw from a Gaussian with mean 7
# and sd 1, we couldn't say dtarget=dnorm, or dtarget=dnorm(x,mean=7,sd=1).
# Instead, we'd have to first define a new one-argument function, as
### dnorm.7.1 <- function(x) {dnorm(x,mean=7,sd=1)}
# and then say dtarget=dnorm.  This is way too clunky.  R does have mechanisms
# for passing arguments along from one function to the other functions it 
# invokes, but they'd distract from getting across the idea of the method.



#### Illustration of the rejection method with a beta distribution
  # See the notes
M <- 3.3
curve(dbeta(x,5,10),from=0,to=1,ylim=c(0,M))
r <- runif(300,min=0,max=1)
u <- runif(300,min=0,max=1)
below <- which(M*u*dunif(r,min=0,max=1) <= dbeta(r,5,10))
points(r[below],M*u[below],pch="+")
points(r[-below],M*u[-below],pch="-")



#### The Arnold cat map, and using it as a pseudo-random number generator

# Arnold cat map function
  # See text of lecture for definition
# Input: a vector v, presumed to be two-dimensional
# Output: the result of applying the map to the vector
arnold.map <- function(v) {
  theta <- v[1]
  phi <- v[2]
  theta.new <- (theta+phi)%%1
  phi.new <- (theta+2*phi)%%1
  return(c(theta.new,phi.new))
}

# Pseudo-random number generator based on the Arnold cat map
  # Iterates applying the map, starting with a "seed" value, and returns the
  # sequence of values for the first coordinate ("theta" in terms of the notes)
# Inputs: number of random variates desired (n)
  # Two-dimensional initial condition vector for the map (seed)
# Output: sequence of n numbers, (approximately) uniformly distributed between
  # 0 and 1
rarnold <- function(n,seed) {
  z <- vector(length=n)
  for (i in 1:n) {
    seed <- arnold.map(seed)
    z[i] <- seed[1]
  }
  return(z)
}






##### The Arnold cat map as a random number generator, illustrated
par(mfrow=c(2,1))
# Get 1000 draws from this generator
z <- rarnold(1000,c(0.11124,0.42111))
# Histogram: it's flat
hist(z,probability=TRUE)
# Scatterplot of successive values --- no predictable pattern, which is exactly
# what we want
plot(z[-1],z[-1000],xlab=expression(Z[t]),ylab=expression(Z[t+1]))




####### Old Faithful data plus OLS regression line
plot(waiting ~ duration, data=geyser,xlab="duration",ylab="waiting")
abline(fit.ols)


##### Simulating from the OLS model fit to the geyser data
  # Holds the values of the independent variable duration fixed, then generates
  # new values of the dependent variable waiting from the linear model plus
  # Gaussian noise
# Inputs: None
# Presumes: fit.ols exists and has the right attributes
# Outputs: A data frame with 299 rows and 2 columns, named duration and
  # waiting
rgeyser <- function() {
  n <- nrow(geyser)
  sigma <- summary(fit.ols)$sigma
  new.waiting <- rnorm(n,mean=fitted(fit.ols),sd=sigma)
  new.geyser <- data.frame(duration=geyser$duration,waiting=new.waiting)
  return(new.geyser)
}


### Comparing the empirical distribution of waiting times to that implied by the
# OLS geyser model
plot(density(geyser$waiting),xlab="waiting",main="",sub="")
lines(density(rgeyser()$waiting),lty=2)


#### Comparing the scatterplot of real data to the simulation of the OLS model
plot(geyser$duration,geyser$waiting,xlab="duration",ylab="waiting")
abline(fit.ols)
points(rgeyser(),pch=20,cex=0.5)





##### Reading in the S&P 500 data ####
sp <- read.csv("SPhistory.short.csv")
# We only want closing prices
sp <- sp[,7]
# The data are in reverse chronological order, which is weird for us
sp <- rev(sp)
# And in fact we only want log returns, i.e., difference in logged prices
sp <- diff(log(sp))


#### Functions for applying method of moments estimation to the MA(1) model

# Method of moments estimator for MA(1)
  # Makes a crude initial guess about the parameters, then minimizes the
  # method-of-moments objective function, using the built-in optimization
  # function optim()
# Inputs: covariance (c), variance (v)
# Calls: ma.mm.objective
# Outputs: The return value of optim(), which begins with the parameters
  # minimizing the objective function (see help(optim) for more)
ma.mm.est <- function(c,v) {
  theta.0 <- c/v
  sigma2.0 <- v
  fit <- optim(par=c(theta.0,sigma2.0), fn=ma.mm.objective, c=c, v=v)
  return(fit)
}

# Calculate the method of moments objective function for MA(1)
  # Euclidean distance between actual covariance and variance, and that
  # implied by the MA(1) model
# Inputs: vector of MA(1) parameters (params)
  # real covariance (c)
  # real variance (v)
# Output: The Euclidean distance
ma.mm.objective <- function(params,c,v) {
  theta <- params[1]
  sigma2 <- params[2]
  c.pred <- theta*sigma2
  v.pred <- sigma2*(1+theta^2)
  return((c-c.pred)^2 + (v-v.pred)^2)
}

#### Simulate an MA(1) model
  # Generate z values, then add them up with appropriate weights to get the x
  # values, which are returned
  # Set up to allow for doing multiple parallel simulation runs
  # In principle, arima.sim() in the stats package can be used instead, but
  # that would be a mere black box
# Inputs: length of the simulation run (n)
  # MA(1) persistence parameter (theta)
  # Variance of z terms (sigma2) - note not their standard deviation
  # Number of independent simulation runs to do (s)
# Outputs: an n by s matrix where each column is an independent MA(1) series
rma <- function(n,theta,sigma2,s=1) {
  z <- replicate(s,rnorm(n=n+1,mean=0,sd=sqrt(sigma2)))
    # n+1 because x[1,] needs two z values
    # replicate(s, foo): if each run of foo gives a vector, replicate makes
    # a matrix with s columns
  x <- z[-1,] + theta*z[-(n+1),] # z[1,] should really be "z[0,]"
  return(x)
}


#### Simulate an MA(1) model and find variance and covariance

# Find the variance of an MA(1) by simulation
# Inputs: length of simulation (n)
  # MA(1) memory parameter (theta)
  # Noise variance sigma2
  # Number of independent simulation paths to average over (s)
# Output: the variance
sim.var <- function(n,theta,sigma2,s=1) {
   vars <- apply(rma(n,theta,sigma2,s),2,var) # Get variance of each column
   # running var on a matrix also calculates covariances across columns,
   # which we don't want
   return(mean(vars))
}

# Find the covariance of an MA(1) by simulation
# Inputs: length of simulation (n)
  # MA(1) memory parameter (theta)
  # Noise variance sigma2
  # Number of independent simulation paths to average over (s)
# Output: the covariance
sim.cov <- function(n,theta,sigma2,s=1) {
  x <- rma(n,theta,sigma2,s)
  covs <- colMeans(x[-1,]*x[-n,])
  return(mean(covs))
}


# Plot covariance, variance, and their ratio as functions of theta in the
# MA(1) model
theta.grid <- seq(from=-1,to=1,length.out=300)
cov.grid <- sapply(theta.grid,sim.cov,sigma2=1,n=length(sp),s=10)
plot(theta.grid,cov.grid,xlab=expression(theta),ylab="Covariance")
abline(0,1,col="grey",lwd=3)
var.grid <- sapply(theta.grid,sim.var,sigma2=1,n=length(sp),s=10)
plot(theta.grid,var.grid,xlab=expression(theta),ylab="Variance")
curve((1+x^2),col="grey",lwd=3,add=TRUE)
plot(theta.grid,cov.grid/var.grid,xlab=expression(theta),
  ylab="Ratio of covariance to variance")
curve(x/(1+x^2),col="grey",lwd=3,add=TRUE)


# Functions for method of simulated moments estimation with an MA(1)

# Method of simulated moments estimator for the MA(1) model
  # Start with a crude parameter guess, then minimize the objective function
  # using simulation to approximate moments, with minimization via
  # optim()
# Inputs: sample covariance (c)
  # sample variance (v)
  # length of simulation paths (n)
  # number of simulation paths per parameter value (s)
# Calls: ma.msm.objective()
# Output: the return value of optim(), including the estimated parameters,
  # see help(optim)
ma.msm.est <- function(c,v,n,s) {
  theta.0 <- c/v
  sigma2.0 <- v
  fit <- optim(par=c(theta.0,sigma2.0),fn=ma.msm.objective,c=c,v=v,n=n,s=s)
  return(fit)
}

# Method of simulated moments objective function
  # Get implied values of the moments by simulation and see how far they are
  # from the data
# Inputs: length-two vector of parameters (params)
  # sample covariance (c)
  # sample variance (v)
  # length of simulation paths (n)
  # number of simulation paths per parameter value (s)
# Calls: sim.cov(), sim.var()
# Output: the distance between the measured moments and the simulation
  # estimate of them
ma.msm.objective <- function(params,c,v,n,s) {
  theta <- params[1]
  sigma2 <- params[2]
  c.pred <- sim.cov(n,theta,sigma2,s)
  v.pred <- sim.var(n,theta,sigma2,s)
  return((c-c.pred)^2 + (v-v.pred)^2)
}
