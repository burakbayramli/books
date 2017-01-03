# Draw a random sample from a standard Gaussian distribution and
# then calculate the KS distance between the empirical CDF and
# a Gaussian CDF --- either the standard Gaussian itself, or a
# Gaussian with mean and variance estimated from the sample
# Designed to illustrate how estimating parameters from data alters
# p-values in the KS test
# Inputs: number of samples, flag for whether to estimate parameters
simul.ks.test.norm <- function(n,estimate=FALSE) {
	x = rnorm(n) # defaults to standard Gaussian
	if (estimate) {
		m = mean(x)
		s = var(x)
	} else {
		m = 0
		s = 1
	}
	as.vector(ks.test(x,pnorm,m,s)$statistic)
	# The "as.vector" part is to get rid of variable names, which
	# are just annoying when we replicate this many times
}

# As with simul.ks.test.norm, only using exponential random 
# variables to show that the distribution of the KS statistic
# becomes independent of the underlying distribution in the
# non-estimating case
simul.ks.test.exp <- function(n,estimate=FALSE) {
	x = rexp(n)
	if (estimate) {
		lambda = 1/mean(x)
	} else {
		lambda = 1
	}
	as.vector(ks.test(x,pexp,lambda)$statistic)
}

# To produce figure in slides:
# fixed.norm.ks.ecdf <- ecdf(replicate(1e4,simul.ks.test.norm(1000)))
# est.norm.ks.ecdf <- ecdf(replicate(1e4,simul.ks.test.norm(1000,estimate=TRUE)))
# fixed.exp.ks.ecdf <- ecdf(replicate(1e4,simul.ks.test.exp(1000)))
# est.exp.ks.ecdf <- ecdf(replicate(1e4,simul.ks.test.exp(1000,estimate=TRUE)))
# curve(1-fixed.norm.ks.ecdf(x),main="Distribution of KS distances",from=0,to=0.1,lwd=5,xlab=expression(d[KS]),ylab="p-value a.k.a. survival function")
# curve(1-fixed.exp.ks.ecdf(x),add=TRUE,col="blue",lwd=2)
## Fatter lines for the fixed-distribution cases so that both show
## up
# curve(1-est.norm.ks.ecdf(x),add=TRUE,col="red")
# curve(1-est.exp.ks.ecdf(x),add=TRUE,col="green")


# Calculate valid p-value for the goodness of fit of a power-law
# tail to a data set, via simulation
# Input: data vector (x), number of replications (m)
# Output: p-value
pareto.tail.ks.test <- function(x,m) {
  x.pt <- pareto.fit(x,threshold="find")
  x0 <- x.pt$xmin # extract parameters of fitted dist.
  alpha <- x.pt$exponent
  ntail <- sum(x>=x0) # How many samples in the tail?
  n <- length(x)
  ptail <- ntail/n # Total prob. of the tail
  # Carve out the non-tail data points
  body <- x[x < x0]
  # Observed value of KS distance:
  d.ks <- ks.dist.for.pareto(x0,x)
  r.ks <- replicate(m,ks.resimulate.pareto.tail(n,ptail,x0,alpha,body))
  # return(r.ks)
  p.value <- sum(r.ks >= d.ks)/m
  return(p.value)
}

# Resimulate from a data set with a Pareto tail, estimate on
# the simulation and report the KS distance
# Inputs: Size of sample (n), probability of being in the tail (tail.p), threshold for tail (threshold), power law exponent (exponent), vector giving values in body (data.body)
# Output: KS distance
ks.resimulate.pareto.tail <- function(n,tail.p,threshold,exponent,data.body) {
  # Samples come from the tail with probability ptail, or
  # else from the body
  # decide randomly how many samples come from the tail
  tail.samples <- rbinom(1,n,tail.p)
  # Draw the samples from the tail
  rtail <- rpareto(tail.samples,threshold,exponent)
  # Draw the samples from the body (with replacement!)
  rbody <- sample(data.body,n-tail.samples,replace=TRUE)
  b <- c(rtail,rbody)
  b.fit <- pareto.fit(b,threshold="find")
  b.x0 <- b.fit$xmin
  b.ks <- ks.dist.for.pareto(b.x0,b)
  return(b.ks)
}

# Draw random values from a distribution with a power-law tail
# With probability p.tail, the variate comes from a Pareto with
# specified exponent and threshold; otherwise it is uniformly
# distributed between 0 and the threshold
rpareto.tail <- function(n,threshold,exponent,p.tail) {
	ntail <- rbinom(1,n,p.tail)
	rtail <- rpareto(ntail,threshold,exponent)
	rbody <- runif(n-ntail,0,threshold)
	r <- c(rtail,rbody)
	# Randomly permute the variates --- see help(sample) for an
	# explanation of why this next command works
	r <- sample(r)
	return(r)
}