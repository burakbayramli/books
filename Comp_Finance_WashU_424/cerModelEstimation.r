# cerModelEstimation.r		scripts for examples in the Estimation of the CER model chapter
#
# author: Eric Zivot
# created: January 30, 2002
# update History: 
# February 6, 2015
#   Separated CER model chapter into 2 chapters and split script file into two.
#   Updated data for examples to be consistent with data in other chapters
# September 6, 2013
#   updated to incorporate comments from Nina Sidneva
# July 26, 2011
#   updated examples for summer 2011.
#
# data for examples
# cerExample		dataframe with month prices on Microsoft, Starbucks and SP500
#					from June 1992 through October 2000. Price data taken from Yahoo
#
# To Do
# 1. write function for fitting CER model: fitCerModel
#
# Shiny App ideas
# 1. Illustrate CLT for estimates - show MC distributions for different sample sizes
# 2. Illustrate bias, se, pdf, and 95% confidence interval coverage by MC

options(digits=3, width=70)
# load R packages
library(PerformanceAnalytics)
library(mvtnorm)
library(tseries)
library(zoo)

# set paths for loading and saving objects
# set globalPath to point to MFTSR directory on local computer
# globalPath = "C:/Users/ezivot.SOCIOLOGY/Dropbox/FinBook/"
globalPath = "C:/Users/ezivot/Dropbox/FinBook/"
setwd(globalPath)
loadPath = paste(globalPath, "Data/", sep="")
savePath = paste(globalPath, "GRAPHS/", sep="")
Rpath = paste(globalPath, "R/", sep="")

#
# plot pdf's for unbiased estimator with high variance, and for
# biased estimator with low variance
#
x.min = -3
x.max = 3
npts = 100
x.vals = seq(from=x.min,to=x.max,length=npts)
pdf1 = dnorm(x.vals,mean=0,sd=1) 
pdf2 = dnorm(x.vals,mean=0.1,sd=0.25)
ylimits = range(pdf1,pdf2)

win.metafile(filename=paste(savePath, "figCerModelEstimation.emf", sep=""))
plot(x.vals,pdf1,ylim=ylimits,type="l",lty=1,ylab="pdf",
     xlab="estimate value", lwd=2)
segments(x0=0, y0=0, x1=0, y1=dnorm(0), lwd=2)
lines(x.vals, pdf2, lty=2, lwd=2, col="red")
legend(x="topleft",legend=c("theta.hat 1","theta.hat 2"), lty=1:2,
       lwd=c(2,2), col=c(1,2))
segments(x0=0.1, y0=0, x1=0.1, y1=dnorm(0.1, mean=0.1, sd=0.25), lwd=2, col="red")
dev.off()

## 
## Ex. Estimating the CER model parameters for Microsoft, Starbucks and the S&P 500 index.
## 

##
## chunk
##
msftPrices = get.hist.quote(instrument="msft", start="1998-01-01",
                            end="2012-05-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
sbuxPrices = get.hist.quote(instrument="sbux", start="1998-01-01",
                            end="2012-05-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
sp500Prices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
colnames(msftPrices) = "MSFT"
colnames(sbuxPrices) = "SBUX"
colnames(sp500Prices) = "SP500"
index(msftPrices) = as.yearmon(index(msftPrices))
index(sbuxPrices) = as.yearmon(index(sbuxPrices))
index(sp500Prices) = as.yearmon(index(sp500Prices))
cerPrices = merge(msftPrices, sbuxPrices, sp500Prices)
msftRetS = Return.calculate(msftPrices, method="simple")
sbuxRetS = Return.calculate(sbuxPrices, method="simple")
sp500RetS = Return.calculate(sp500Prices, method="simple")
cerRetS = Return.calculate(cerPrices, method="simple")
msftRetS = msftRetS[-1]
sbuxRetS = sbuxRetS[-1]
sp500RetS = sp500RetS[-1]
cerRetS = cerRetS[-1]
msftRetC = log(1 + msftRetS)
sbuxRetC = log(1 + sbuxRetS)
sp500RetC = log(1 + sp500RetS)
cerRetC = merge(msftRetC, sbuxRetC, sp500RetC)
colnames(cerRetC) = c("MSFT", "SBUX", "SP500")

# 
# estimated parameters from CER model
#

##
## chunk
##
muhat = apply(cerRetC,2,mean)
muhat
##
## chunk
##
sigma2hat = apply(cerRetC,2,var)
sigma2hat
sigmahat = apply(cerRetC,2,sd)
sigmahat
##
## chunk
##
covmat = var(cerRetC)
covmat
cormat = cor(cerRetC)
cormat
##
## chunk
##
covhat = covmat[lower.tri(covmat)]
rhohat = cormat[lower.tri(cormat)]
names(covhat) <- names(rhohat) <- 
c("msft,sbux","msft,sp500","sbux,sp500")
covhat
rhohat

##
## End Ex.
##


##
## Ex. se(mu) values for Microsoft, Starbucks and the S&P 500 index
##

# compute estimated standard error for mean
n.obs = nrow(cerRetC)
seMuhat = sigmahat/sqrt(n.obs)
# show estimates with standard errors
cbind(muhat, seMuhat)
# compute t-ratios
seMuhat/muhat

# compute 95% confidence intervals
upper = muhat + 2*seMuhat
lower = muhat - 2*seMuhat
width = upper - lower
cbind(lower, upper, width)

##
## Ex. se values of sigmahat, sigma2hat and rhohat for Microsoft, Starbucks and the S&P 500 index
##


# compute estimated standard errors for variance and sd
seSigma2hat = sigma2hat/sqrt(n.obs/2)
seSigmahat = sigmahat/sqrt(2*n.obs)
cbind(sigma2hat, seSigma2hat, sigmahat, seSigmahat)
      
seRhohat = (1-rhohat^2)/sqrt(n.obs)
cbind(rhohat, seRhohat)


##
## Ex. 95% confidence intervals for {i} for Microsoft, Starbucks and the S & P 500 index.
##

# 2.5% t-quantile
t.975 = qt(0.975, df=(n.obs-1))
t.975

# compute exact 95% confidence intervals
lower = muhat - t.975*seMuhat
upper = muhat + t.975*seMuhat
width = upper - lower
cbind(lower, upper, width)

# compute approx 95% confidence intervals
lower = muhat - 2*seMuhat
upper = muhat + 2*seMuhat
width = upper - lower
cbind(lower,upper,width)


# compute approx 95% confidence intervals
lowerSigma2 = sigma2hat - 2*seSigma2hat
upperSigma2 = sigma2hat + 2*seSigma2hat
widthSigma2 = upperSigma2 - lowerSigma2
cbind(lowerSigma2, upperSigma2, widthSigma2)

lowerSigma = sigmahat - 2*seSigmahat
upperSigma = sigmahat + 2*seSigmahat
widthSigma = upperSigma - lowerSigma
cbind(lowerSigma, upperSigma, widthSigma)


# compute approx 95% confidence intervals
lowerRho = rhohat - 2*seRhohat
upperRho = rhohat + 2*seRhohat
widthRho = upperRho - lowerRho
cbind(lowerRho, upperRho, widthRho)


##
## EX. evaluate sampling distribution of muhat
##
x.vals = seq(3, -3, length=100)
pdf1 = dnorm(x.vals)
pdf2 = dnorm(x.vals, sd=1/sqrt(10))
pdf3 = dnorm(x.vals, sd=1/sqrt(50))
ub = max(pdf1, pdf2, pdf3)

# Figure
win.metafile(filename=paste(savePath, "figCerModelEstimationCerMuhat.emf", sep=""))
plot(x.vals, pdf1, ylab="mu", ylim=c(0, ub), type="l", lwd=2)
lines(x.vals, pdf2, lwd=2, col=2, lty=2)
lines(x.vals, pdf3, lwd=2, col=3, lty=3)
legend(x="topleft", legend=c("pdf: T=1", "pdf: T=10", "pdf: T=50"),
       lty=1:3, col=1:3, lwd=2)
dev.off()

##
## End Ex.
##

#
# Monte Carlo simulation of CER model for single asset
#

# generate 10 simulated samples from CER model and plot
# r = 0.05 + e, e ~ iid N(0, 0.10^2)
mu = 0.05
sd = 0.10
n.obs = 100
n.sim = 10
set.seed(111)
sim.ret = matrix(0, n.obs, n.sim)
for (sim in 1:n.sim) {
	sim.ret[,sim] = rnorm(n.obs,mean=mu,sd=sd)
}

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
# Figure
win.metafile(filename=paste(savePath, "figCerModelEstimationCerMcSim.emf", sep=""))
plot(as.zoo(sim.ret), col="blue", lwd=2, main="", panel=my.panel)
dev.off()


# generate 1000 samples from CER and evaluate sampling properties of muhat
mu = 0.05
sigma = 0.10
n.obs = 100
n.sim = 1000
set.seed(111)
sim.means = rep(0,n.sim)	# initialize vectors
mu.lower = rep(0,n.sim)
mu.upper = rep(0,n.sim)
qt.975 = qt(0.975, n.obs-1)
for (sim in 1:n.sim) {
	sim.ret = rnorm(n.obs,mean=mu,sd=sigma)
	sim.means[sim] = mean(sim.ret)
	se.muhat = sd(sim.ret)/sqrt(n.obs)
	mu.lower[sim] = sim.means[sim]-qt.975*se.muhat
	mu.upper[sim] = sim.means[sim]+qt.975*se.muhat
}
mean(sim.means)
sd(sim.means)
in.interval = mu >= mu.lower & mu <= mu.upper
sum(in.interval)/n.sim

# compute expected value of estimates and bias
mean(sim.means)
mean(sim.means) - mu
sd(sim.means)
# true se(muhat)
sigma/sqrt(n.obs)

# plot histogram and overlay true sampling distribution
win.metafile(filename=paste(savePath, "figCerModelEstimationMcMuhat.emf", sep=""))
hist(sim.means, col="cornflowerblue", ylim=c(0,40), 
     main="", xlab="muhat", probability=T)
abline(v=mean(sim.means), col="white", lwd=4, lty=2)
# overlay normal curve
x.vals = seq(0.02, 0.08, length=100)
lines(x.vals, dnorm(x.vals, mean=mu, sd=sigma/sqrt(100)), col="orange", lwd=2)
dev.off()

# MC simulation to evaluate statistical properties of sigma2hat and sigmahat
mu = 0.05
sigma = 0.10
n.obs = 100
n.sim = 1000
set.seed(111)
sim.means = rep(0,n.sim)	# initialize vectors
sim.sigma2 = rep(0,n.sim)
sigma2.lower = rep(0,n.sim)
sigma2.upper = rep(0,n.sim)
sim.sigma = rep(0,n.sim)
sigma.lower = rep(0,n.sim)
sigma.upper = rep(0,n.sim)
for (sim in 1:n.sim) {
	sim.ret = rnorm(n.obs,mean=mu,sd=sigma)
	sim.means[sim] = mean(sim.ret)
	sim.sigma2[sim] = var(sim.ret)
	sim.sigma[sim] = sqrt(sim.sigma2[sim])
  sigma2.lower[sim] = sim.sigma2[sim] - 2*sim.sigma2[sim]/sqrt(n.obs/2)
	sigma2.upper[sim] = sim.sigma2[sim] + 2*sim.sigma2[sim]/sqrt(n.obs/2)
	sigma.lower[sim] = sim.sigma[sim] - 2*sim.sigma[sim]/sqrt(n.obs*2)
	sigma.upper[sim] = sim.sigma[sim] + 2*sim.sigma[sim]/sqrt(n.obs*2)
}

# compute expected value of estimates and bias
mean(sim.means)
mean(sim.means) - mu
mean(sim.sigma2)
mean(sim.sigma2) - sigma^2
mean(sim.sigma)
mean(sim.sigma) - sigma

in.interval = sigma^2 >= sigma2.lower & sigma^2 <= sigma2.upper
sum(in.interval)/n.sim

in.interval = sigma >= sigma.lower & sigma <= sigma.upper
sum(in.interval)/n.sim


# Figure
win.metafile(filename=paste(savePath, "figCerModelEstimationMcSigma.emf", sep=""))
par(mfrow=c(1,2))
	hist(sim.sigma2, col="cornflowerblue", xlab="sigma2 hat values", 
       main="sigma2 hat", probability=T)
	abline(v=mean(sim.sigma2), col="white", lwd=4, lty=2)
  # overlay normal distribution
  x.vals = seq(0.005, 0.016, length=100)
  lines(x.vals, dnorm(x.vals, mean=sigma^2, sd=sigma^2/sqrt(100/2)), col="orange", lwd=2)
	hist(sim.sigma, col="cornflowerblue", xlab="sigma hat values", 
       main="sigma hat", probability=T)
	abline(v=mean(sim.sigma), col="white", lwd=4, lty=2)
  # overlay normal distribution
  x.vals = seq(0.07, 0.13, length=100)
  lines(x.vals, dnorm(x.vals, mean=sigma, sd=sigma/sqrt(2*100)), col="orange", lwd=2)
par(mfrow=c(1,1))
dev.off()

#
# compute Monte Carlo standard errors and compare with
# asymptotic formulas based on true values
#

sd(sim.means)
sigma/sqrt(n.obs)
sd(sim.sigma2)
sigma^2/sqrt(n.obs/2)
sd(sim.sigma)
sigma/sqrt(2*n.obs)


#
# use Monte Carlo to evaluate confidence interval coverage
#

# generate 1000 samples from CER and compute sample statistics
n.sim = 1000
set.seed(111)
mu.lower = rep(0,n.sim)	# initialize vectors
mu.upper = rep(0,n.sim)
for (sim in 1:n.sim) {
	sim.ret = rnorm(n.obs,mean=mu,sd=sd)
	mu.hat = mean(sim.ret)
	se.muhat = sd(sim.ret)/sqrt(n.obs)
	mu.lower[sim] = mu.hat - 2*se.muhat
	mu.upper[sim] = mu.hat + 2*se.muhat
}
in.interval = (mu >= mu.lower) & (mu <= mu.upper)
sum(in.interval)/n.sim



# generate 1000 samples from CER and compute correlations
n.obs = 100
n.sim = 1000
set.seed(111)
sim.corrs = rep(0, n.sim)	# initialize vectors

muvec = c(0.05, 0.03)
sigmavec = c(0.1, 0.05)
names(muvec) = names(sigmavec) = c("Asset.1", "Asset.2")
rho12 = 0.75
Sigma = diag(sigmavec^2)
Sigma[1,2] = Sigma[2,1] = rho12*sigmavec[1]*sigmavec[2]
rho.lower = rep(0,n.sim)  
rho.upper = rep(0,n.sim)
for (sim in 1:n.sim) {
	sim.ret = rmvnorm(n.obs,mean=muvec, sigma=Sigma)
	sim.corrs[sim] = cor(sim.ret)[1,2]
	se.rhohat = (1 - sim.corrs[sim]^2)/sqrt(n.obs)
	rho.lower[sim] = sim.corrs[sim] - 2*se.rhohat
	rho.upper[sim] = sim.corrs[sim] + 2*se.rhohat
}
in.interval = (rho12 >= rho.lower) & (rho12 <= rho.upper)
sum(in.interval)/n.sim

# Figure
win.metafile(filename=paste(savePath, "figCerModelEstimationMcRho.emf", sep=""))
hist(sim.corrs, xlab="rhohat values", col="cornflowerblue", 
     main="rhohat", probability=TRUE)
abline(v=rho12, lwd=4, col="white")
x.vals = seq(0.5, 0.9, length=100)
lines(x.vals, dnorm(x.vals, mean=rho12, sd=(1-rho12^2)/sqrt(n.obs)), col="orange", lwd=2)
dev.off()

# MC means and standard deviations
mean(sim.corrs)
mean(sim.corrs) - rho12
sd(sim.corrs)
(1-rho12^2)/sqrt(n.obs)

#
# show se from qhat for simple returns
#

sigma = 0.10
n.obs = 60
se.qhat = function(x, sigma, n.obs) {
  (sigma/sqrt(n.obs))*sqrt(1 + 0.5*qnorm(x)^2)
}

a.vals = seq(0.005, 0.05, length.out=100)
plot(a.vals, se.qhat(a.vals, sigma, n.obs), 
     ylab="se", xlab="alpha", type="l", lwd=2, col="cornflowerblue")


#
# Estimate Quantiles and VaR from CER model
#

# use R function qnorm to compute quantiles from standard normal distribution
n.obs = length(msftRetC)
muhatS = colMeans(cerRetS)
sigmahatS = apply(cerRetS, 2, sd)
qhat.05 = muhatS + sigmahatS*qnorm(0.05)
qhat.01 = muhatS + sigmahatS*qnorm(0.01)
seQhat.05 = (sigmahatS/sqrt(n.obs))*sqrt(1 + 0.5*qnorm(0.05)^2)
seQhat.01 = (sigmahatS/sqrt(n.obs))*sqrt(1 + 0.5*qnorm(0.01)^2)
cbind(qhat.05, seQhat.05, qhat.01, seQhat.01)

# 95% confidence intervals
lowerQhat.05 = qhat.05 - 2*seqhat.05
upperQhat.05 = qhat.05 + 2*seqhat.05
widthQhat.05 = upperQhat.05 - lowerQhat.05
cbind(lowerQhat.05, upperQhat.05, widthQhat.05)

lowerQhat.01 = qhat.01 - 2*seqhat.01
upperQhat.01 = qhat.01 + 2*seqhat.01
widthQhat.01 = upperQhat.01 - lowerQhat.01
cbind(lowerQhat.01, upperQhat.01, widthQhat.01)


#
# estimate VaR from simple returns
#
w0=100000
VaR.05 = qhat.05*w0
seVaR.05 = w0*seQhat.05
VaR.01 = qhat.01*w0
seVaR.01 = w0*seQhat.01
cbind(VaR.05, seVaR.05, VaR.01, seVaR.01)

# 95% confidence intervals
lowerVaR.05 = VaR.05 - 2*seVaR.05
upperVaR.05 = VaR.05 + 2*seVaR.05
widthVaR.05 = upperVaR.05 - lowerVaR.05
cbind(lowerVaR.05, upperVaR.05, widthVaR.05)

lowerVaR.01 = VaR.01 - 2*seVaR.01
upperVaR.01 = VaR.01 + 2*seVaR.01
widthVaR.01 = upperVaR.01 - lowerVaR.01
cbind(lowerVaR.01, upperVaR.01, widthVaR.01)

# estimate VaR using cc returns
W0 = 100000
VaRhat.05 = (exp(qhat.05)-1)*W0
VaRhat.05

# VaR example from slides
muhat = 0.02
sigmahat = 0.10
qhat.05 = muhat + sigmahat*qnorm(0.05)
W0 = 10000
VaRhat.05 = (exp(qhat.05)-1)*W0
VaRhat.05

