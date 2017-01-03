# maximumLikelihood.r
#
# created: July 19, 2012
# revision history

options(digits=4, width=70)
library(maxLik)

#
# Bernoulli example
#

# Bernoulli likelihood
# x = 0, 1
# f(x,theta) = (theta^x)*(1-theta)^(1-x)

likelihood.Bernoulli = function(theta, x) {
  # theta   success probability parameter
  # x       vector of data
  n = length(x)
  ans = theta^sum(x) * (1-theta)^(n-sum(x))
  return(ans)
}

# plot Bernoulli likelihood
x = rep(0,5)
theta.vals = seq(0,1, length.out=10)
like.vals = likelihood.Bernoulli(theta.vals, x)
plot(theta.vals, like.vals, type="b", col="blue", lwd=2,
     main="Bernoulli Likelihood for x=(0,0,0,0,0)")

x = rep(1,5)
like.vals = likelihood.Bernoulli(theta.vals, x)
plot(theta.vals, like.vals, type="b", col="blue", lwd=2,
     main="Bernoulli Likelihood for x=(1,1,1,1,1)")

#
# normal likelihood
#

likelihood.normal = function(theta, x) {
  # theta   vector of normal distribution parameters
  #         theta = (mu, sig2)'
  # x       vector of data
  mu = theta[1]
  sig2 = theta[2]
  n = length(x)
  a1 = (2*pi*sig2)^-(n/2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1*exp(a2*sum(y))
  return(ans) 
}

likelihood.normal.mu = function(mu, sig2=1, x) {
  # mu      mean of normal distribution for given sig
  # x       vector of data
  n = length(x)
  a1 = (2*pi*sig2)^-(n/2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1*exp(a2*sum(y))
  return(ans) 
}


# generate N(0,1) data
n = 50
set.seed(123)
x = rnorm(n, mean=0, sd=1)

# compute normal likelihood as function of mu
mu.vals = seq(-1,1, length.out=100)
like.vals = rep(0,length(mu.vals))
for (i in 1:length(like.vals)) {
  like.vals[i] = likelihood.normal.mu(mu.vals[i], sig2=1, x=x)
}

plot(mu.vals, like.vals, type="l", col="blue", lwd=2)
abline(v=0, col="red", lwd=2)
mean(x)
#
# log-likelihood functions
#

# Bernoulli log-likelihood
# x = 0, 1
# f(x,theta) = (theta^x)*(1-theta)^(1-x)

loglike.Bernoulli = function(theta, x) {
  # theta   success probability parameter
  # x       vector of data
  n = length(x)
  ans = log(theta)*sum(x)+log(1-theta)*(n-sum(x))
  return(ans)
}

# plot Bernoulli log-likelihood
par(mfrow=c(1,2))
x1 = rep(0,5)
theta.vals = seq(0.1,0.99, length.out=10)
loglike.vals = loglike.Bernoulli(theta.vals, x1)
plot(theta.vals, loglike.vals, type="b", col="blue", lwd=2,
     main="Bernoulli log-likelihood for x=(0,0,0,0,0)")

x2 = rep(1,5)
like.vals = likelihood.Bernoulli(theta.vals, x2)
plot(theta.vals, like.vals, type="b", col="blue", lwd=2,
     main="Bernoulli Likelihood for x=(1,1,1,1,1)")
par(mfrow=c(1,1))

# normal log-likelihood
loglike.normal.mu = function(mu, sig2=1, x) {
  # mu      mean of normal distribution for given sig
  # x       vector of data
  n = length(x)
  a1 = -(n/2)*log(2*pi)-(n/2)*log(sig2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1+a2*sum(y)
  return(ans) 
}

# generate N(0,1) data
n = 50
set.seed(123)
x = rnorm(n, mean=0, sd=1)

# compute normal likelihood as function of mu
mu.vals = seq(-1,1, length.out=100)
loglike.vals = rep(0,length(mu.vals))
for (i in 1:length(loglike.vals)) {
  loglike.vals[i] = loglike.normal.mu(mu.vals[i], sig2=1, x=x)
}

plot(mu.vals, loglike.vals, type="l", col="blue", lwd=2)
abline(v=0, col="red", lwd=2)


#
# Optimization in R
#

# optimize() function
test.fun = function(x) {
  return(x^2)
}

ans = optimize(test.fun, lower=-1, upper=1, maximum=FALSE)
class(ans)
names(ans)
ans

x.vals = seq(-1,1,length.out=100)
plot(x.vals, test.fun(x.vals), type="l", col="blue",
     xlab="x", ylab="f(x)", main="f(x)=x^2")

# use optim() to minimize functions of multiple variables
test.fun = function(theta) {
  ans = theta[1]^2 + theta[2]^2
  return(ans)
}

# set starting values for optimizer
theta.start = c(1,1)
ans = optim(par=theta.start, fn=test.fun,
            method="BFGS")
class(ans)
names(ans)
ans

# maximize normal log-likelihood using optim
# by minimizing -1*log-likelihood
loglike.normal = function(theta, x) {
  # theta   parameters c(mu,sig2)
  # x       vector of data
  mu = theta[1]
  sig2 = theta[2]
  n = length(x)
  a1 = -(n/2)*log(2*pi)-(n/2)*log(sig2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1+a2*sum(y)
  # return -1 * loglike
  return(-ans) 
}

# generate N(0,1) data
n = 50
set.seed(123)
x = rnorm(n, mean=0, sd=1)
# set starting values for optimizer
theta.start = c(0,1)
ans = optim(par=theta.start, fn=loglike.normal, x=x,
            method="BFGS")
ans$par

# verify mle is mean and scaled variance
mean(x)
var(x)*(n-1)/n

# compute MLE and get standard errors
# set starting values for optimizer
ans = optim(par=theta.start, fn=loglike.normal, x=x,
            method="BFGS", hessian=TRUE)
names(ans)
ans$hessian
se.mle = sqrt(diag(solve(ans$hessian)))
se.mle


# use maxLik function from maxLik package
# here function to compute log-likelihood returns log-likelihood values
# and not -1*log-likelihood values
library(maxLik)
loglike.normal = function(theta, x) {
  # theta   parameters c(mu,sig2)
  # x       vector of data
  mu = theta[1]
  sig2 = theta[2]
  n = length(x)
  a1 = -(n/2)*log(2*pi)-(n/2)*log(sig2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1+a2*sum(y)
  return(ans) 
}
theta.start = c(0,1)
names(theta.start) = c("mu","sig2")
theta.mle = maxLik(loglike.normal, start=theta.start, x=x)
class(theta.mle)
names(theta.mle)
theta.mle
summary(theta.mle)