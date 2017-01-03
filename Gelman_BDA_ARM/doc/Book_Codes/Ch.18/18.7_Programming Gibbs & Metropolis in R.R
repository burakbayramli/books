## The simulation algorithim

 # log likelihood function
f.loglik <- function (y, a, b , o, data.n){
  theta.mat <- exp (outer (a, b, "+"))
  0.mat <- outer (rep (1, data.n), o, "*")
  A.mat <- theta.mat/(0.mat-1)   # the "alpha" and "beta" parameters
  B.mat <- 1/(0.mat-1)           # of the negative binomial distribution
  loglik <- lgamma(y+A.mat) - lgamma(A.mat) - lgamma(y+1) +
     (log(B.mat)-log(B.mat+1))*A.mat - log(B.mat+1)*y
  return (loglik)
}

 # log posterior density function
f.logpost.a <- function (){
  loglik <- f.loglik (y, a, b , o, data.n)
  rowSums (loglik, na.rm=TRUE) + dnorm (a, mu.a, sigma.a, log=TRUE)
}
f.logpost.b <- function (){
  loglik <- f.loglik (y, a, b , o, data.n)
  colSums (loglik, na.rm=TRUE) + dnorm (b, mu.b, sigma.b, log=TRUE)
}
f.logpost.o <- function (){
  reject <- !(o>1)         # reject if omega is not greater than 1
  o[reject] <- 2           # set rejected omega's to arbitrary value of 2
  loglik <- f.loglik (y, a, b , o, data.n)
  loglik <- colSums (loglik, na.rm=TRUE) -2*log(o)
  loglik[reject] <- -Inf   # set loglik to zero for rejected values
  return (loglik)
}

 # data and initial values
library ("foreign")
y <- as.matrix (read.dta ("all.dta"))
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/?

 # define initial values for parameters
a.init <- function() {rnorm (data.n)}
b.init <- function() {rnorm (data.j)}
o.init <- function() {runif (data.j, 1.01, 50)}
mu.a.init <- function() {rnorm (1)}
mu.b.init <- function() {rnorm (1)}
sigma.a.init <- function() {runif (1)}
sigma.b.init <- function() {runif (1)}

 # Gibbs sampler steps
mu.a.update <- function(){
  rnorm (1, mean(a), sigma.a/sqrt(data.n))
}
mu.b.update <- function(){
  rnorm (1, mean(b), sigma.b/sqrt(data.j))
}
sigma.a.update <- function(){
  sqrt (sum((a-mu.a)^2)/rchisq(1, data.n-1))
}
sigma.b.update <- function(){
  sqrt (sum((b-mu.b)^2)/rchisq(1, data.j-1))
}

 # randomization step
renorm.network <- function(){
  const <- log (sum(exp(b[c(2,4,12)]))/.00357) +
     .5*log (sum(exp(b[c(3,7)]))/.00760) - 
     .5*log (sum(exp(b[c(6,8,10)]))/.00811)
  a <- a + const 
  mu.a <- mu.a + const 
  b <- b - const 
  mu.b <- mu.b - const 
}

 # setting up the Umacs sampler function
s.network <- Sampler (
y = y,
data.n = nrow(y),
data.j = ncol(y),
a = PSMetropolis (f.logpost.a, a.init),
b = PSMetropolis (f.logpost.b, b.init),
o = PSMetropolis (f.logpost.o, o.init),
mu.a = Gibbs (mu.a.update, mu.a.init),
mu.b = Gibbs (mu.b.update, mu.b.init),
sigma.a = Gibbs (sigma.a.update, sigma.a.init),
sigma.b = Gibbs (sigma.b.update, sigma.b.init),
renorm.network)

 # running Umacs and saving the simulations
network.1 <- s.network (n.iter=2000, n.sims=1000, n.chains=3)
network.1.bugs <- as.bugs.array (network.1)
plot (network.1)

attach.bugs (network.1.bugs)



