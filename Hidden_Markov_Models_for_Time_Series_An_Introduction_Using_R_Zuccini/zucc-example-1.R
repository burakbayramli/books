###
# fit independent mixtures of Poissons to quakes dataset
# from Zucchini & MacDonald Hidden Markov Models for Timeseries exercise 1.3
# see http://petewerner.blogspot.com/2014/12/fitting-mixture-of-independent-poisson.html
##

#quakes data
xf <- c(13, 14, 8, 10, 16, 26, 32, 27, 18, 32, 36, 24, 22, 23, 22, 18, 25, 21, 21, 14, 8, 11, 14, 23, 18, 17, 19, 20, 22, 19, 13, 26, 13, 14, 22, 24, 21, 22, 26, 21, 23, 24, 27, 41, 31, 27, 35, 26, 28, 36, 39, 21, 17, 22, 17, 19, 15, 34, 10, 15, 22, 18, 15, 20, 15, 22, 19, 16, 30, 27, 29, 23, 20, 16, 21, 21, 25, 16, 18, 15, 18, 14, 10, 15, 8, 15, 6, 11, 8, 7, 18, 16, 13, 12, 13, 20, 15, 16, 12, 18, 15, 16, 13, 15, 16, 11, 11)


udist <- function(n) rep(1/n, n) 

#natural to working
n2wp <- function(p) {
	m <- length(p)
	log(p[2:m]/(1 - sum(p[2:m])))
}

#working to natural
w2np <- function(lp) {
	rv <- exp(lp)/(1 + sum(exp(lp)))	
	c(1 - sum(rv), rv)
}

#optimisation function
of <- function(pv, m, x) {
	if (m == 1) 
		return(-sum(dpois(x, exp(pv), log=TRUE)))
	#convert working parameters to natural paramters
	pr <- exp(pv[1:m])
	probs <- w2np(pv[(m+1):(2*m - 1)])
	#calculate -ve log likelihood
	-sum(log(outer(x, pr, dpois) %*% probs))
}

#initial estimates and probabilities for 2, 3 and 4 distributions
#the lambda values I just guess, and use an uniform distribution
#for the initial mixing distribution.
pv <- c(log(c(10, 20)), n2wp(udist(2)))
pv <- c(log(c(10, 15, 20)), n2wp(udist(3)))
pv <- c(log(c(5, 15, 20, 30)), n2wp(udist(4)))

#number of distributions to fit
m <- (length(pv) + 1)/2

#fit using nlm
fv <-nlm(of, pv, m, xf, print.level=0) 
rv <- fv$est

#lambda estimates
exp(rv[1:m])
#mixing distribution
w2np(rv[(m+1):(2*m-1)])
