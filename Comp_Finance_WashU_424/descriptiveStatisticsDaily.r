# DescriptiveStatisticsDaily.r
#
# author: Eric Zivot
# created: October 11, 2006
# update history: 
# July 16, 2013
#   Updated for summer 2013
# July 11, 2012
#   Updated data and examples for summer 2012
# October 25, 2009
#   Added VaR calculations
#
# R functions used
#
# abline
# apply
# chart.TimeSeries
# class
# colnames
# diff
# end
# get.hist.quote
# log
# par
# plot 
# start
#

options(digits=4, width=70)

# load packages
library("TSA")
library("tseries")
library(PerformanceAnalytics)
library("zoo")

# get monthly adjusted closing price data on MSFT and SP500 from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# Dec 2007. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

msftDailyPrices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="zoo")
class(msftDailyPrices)
colnames(msftDailyPrices)
start(msftDailyPrices)
end(msftDailyPrices)
colnames(msftDailyPrices) = "MSFT"


sp500DailyPrices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="zoo")
colnames(sp500DailyPrices) = "SP500"

# plot daily prices
par(mfrow=c(2,1))
plot(msftDailyPrices, main="MSFT daily closing price", ylab="price", col="blue")      
plot(sp500DailyPrices, main="SP500 daily closing price", ylab="price", col="blue")
par(mfrow=c(1,1))

# number of observations
nrow(msftDailyPrices)

#
# compute daily cc returns
#

msftDailyReturns = diff(log(msftDailyPrices))
sp500DailyReturns = diff(log(sp500DailyPrices))
DailyReturns = merge(msftDailyReturns, sp500DailyReturns)
# create matrix data 
msftDailyReturns.mat = coredata(msftDailyReturns)
sp500DailyReturns.mat = coredata(sp500DailyReturns)
DailyReturns.mat = coredata(DailyReturns)

# plot daily returns
plot(DailyReturns)
plot(msftDailyReturns, col="blue", main="Daily cc returns on MSFT")
abline(h=0)
plot(sp500DailyReturns, col="blue", main="Daily cc returns on SP500")
abline(h=0)

# use PerformanceAnalytics function chart.TimeSeries for a nicer plot
chart.TimeSeries(msftDailyReturns, main="Daily cc returns on MSFT",
                 col="blue", lwd=1, ylab="Return")
chart.TimeSeries(sp500DailyReturns, main="Daily cc returns on SP500",
                 col="blue", lwd=1, ylab="Return")


# compute descriptive statistics
apply(DailyReturns.mat, 2, mean)
apply(DailyReturns.mat, 2, sd)
apply(DailyReturns.mat, 2, skewness)
apply(DailyReturns.mat, 2, kurtosis)


#
# histograms with normal density overlayed
#

hist(msftDailyReturns.mat, main="MSFT Daily Returns with Calibrated Normal curve",
     probability=T, col="slateblue1")
x.vals = seq(-0.15, 0.15, length=100)
lines(x.vals, dnorm(x.vals, mean=mean(msftDailyReturns.mat), 
                    sd=sd(msftDailyReturns.mat)), col="orange", lwd=2)
hist(sp500DailyReturns.mat, main="SP500 Daily Returns with Calibrated Normal curve",
     probability=T, col="slateblue1", ylim=c(0, 30), breaks=15)
x.vals = seq(-0.08, 0.08, length=100)
lines(x.vals, dnorm(x.vals, mean=mean(sp500DailyReturns.mat), 
                    sd=sd(sp500DailyReturns.mat)), col="orange", lwd=2)
     
# put histograms on same graph with same scaling
MSFT.breaks = hist(msftDailyReturns.mat, plot=F)
par(mfrow=c(2,1))
	hist(msftDailyReturns.mat, main="MSFT daily returns", 
           probability=T, col="slateblue1", xlab="")
	x.vals = seq(-0.15, 0.15, length=100)
	lines(x.vals, dnorm(x.vals, mean=mean(msftDailyReturns.mat), 
      	              sd=sd(msftDailyReturns.mat)), col="orange", lwd=2)
	hist(sp500DailyReturns.mat, main="SP500 daily returns", col="slateblue1",
	     breaks=MSFT.breaks$breaks, probability=T, ylim=c(0,35), xlab="")
	lines(x.vals, dnorm(x.vals, mean=mean(sp500DailyReturns.mat), 
                    sd=sd(sp500DailyReturns.mat)), col="orange", lwd=2)

par(mfrow=c(1,1))

#
# boxplot
#
boxplot(DailyReturns.mat, main="Daily Returns", col="slateblue")
abline(h=0)

#
# historical VaR
#

# MSFT
q.01 = quantile(msftDailyReturns.mat, probs=0.01)
q.05 = quantile(msftDailyReturns.mat, probs=0.05)
q.01
q.05
VaR.01 = 100000*(exp(q.01) - 1)
VaR.05 = 100000*(exp(q.05) - 1)
VaR.01
VaR.05

# SP500
q.01 = quantile(sp500DailyReturns.mat, probs=0.01)
q.05 = quantile(sp500DailyReturns.mat, probs=0.05)
q.01
q.05
VaR.01 = 100000*(exp(q.01) - 1)
VaR.05 = 100000*(exp(q.05) - 1)
VaR.01
VaR.05

# normal quantiles
qnorm(0.01, mean=mean(msftDailyReturns.mat),
      sd=sd(msftDailyReturns.mat))
qnorm(0.05, mean=mean(msftDailyReturns.mat),
      sd=sd(msftDailyReturns.mat))

#
# qq-plots
#

par(mfrow=c(1,2))
qqnorm(msftDailyReturns.mat, sub="MSFT", col="blue")
qqline(msftDailyReturns.mat)

qqnorm(sp500DailyReturns.mat, sub="SP500", col="blue")
qqline(sp500DailyReturns.mat)
par(mfrow=c(1,1))

#
# smoothed density with normal density superimposed
#

# bug in density if timeSeries object is used
msft.density = density(msftDailyReturns.mat, n=100, from=-0.17, to=0.17)
plot(msft.density, type="l", lwd=2, ylab="Density estimates", 
     xlab="Daily returns on Microsoft", main="MSFT")
points(msft.density$x, dnorm(msft.density$x, mean=mean(msftDailyReturns.mat),
       sd=sd(msftDailyReturns.mat)), type="l", lwd=2, col=5)
legend(x="topleft", legend=c("Smoothed density", "Normal density"), lty=c(1,1), 
       col=c(1,5), lwd=c(2,2))       

sp500.density = density(sp500DailyReturns.mat, n=100, from=-0.07, to=0.07)
plot(sp500.density, type="l", lwd=2, ylab="Density estimates", 
     xlab="Daily returns on S&P 500 Index", main="SP500")
points(sp500.density$x, dnorm(sp500.density$x, mean=mean(sp500DailyReturns.mat),
       sd=sd(sp500DailyReturns.mat)), type="l", lwd=2, col=5)
legend(-0.06, 40, legend=c("Smoothed density", "Normal density"), lty=c(1,1), 
       col=c(1,5), lwd=c(2,2))       

#
# time series descriptive statistics
#

# compare Calibrated Gaussian White noise to actural returns
set.seed(123)
gwn = rnorm(length(msftDailyReturns.mat), mean=mean(msftDailyReturns.mat),
            sd=sd(msftDailyReturns.mat))
par(mfrow=c(2,1))
	ts.plot(msftDailyReturns.mat, ylab="MSFT", main="MSFT daily returns", 
              col="blue")
	abline(h=0)
	ts.plot(gwn, main="Gaussian data calibrated to MSFT", col="blue") 
	abline(h=0)
par(mfrow=c(1,1))

# Sample ACF to show lack of time dependence in daily returns
acf(msftDailyReturns.mat)
acf(sp500DailyReturns.mat)


# GWN has independent returns. Hence, any transformation of GWN is also independent
par(mfrow=c(2,1))
	ts.plot(abs(gwn), main="Absolute values of GWN", col="blue")
	ts.plot(gwn^2, main="Squared values of GWN", col="blue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
tmp = acf(abs(gwn), type="correlation")
tmp = acf(gwn^2, type="correlation")
par(mfrow=c(1,1))


# Let's see if MSFT returns are independent - look at time dependence in abs() and
# squares.
par(mfrow=c(2,1))
	chart.TimeSeries(abs(msftDailyReturns), main="Daily absolute returns on Microsoft",
           col="blue", ylab="abs(returns)", lwd=1)
	chart.TimeSeries(msftDailyReturns^2, main="Daily squared returns on Microsoft",
           col="blue", ylab="(returns)^2", lwd=1)
par(mfrow=c(1,1))

par(mfrow=c(2,1))
  acf(abs(msftDailyReturns.mat), main="MSFT absolute returns")
  acf(msftDailyReturns.mat^2, main="MSFT squared returns")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
	chart.TimeSeries(abs(sp500DailyReturns), main="Daily absolute returns on S&P 500",
           col="blue", lwd=1, ylab="abs(returns)")
	chart.TimeSeries(sp500DailyReturns^2, main="Daily squared returns on S&P 500",
           col="blue", lwd=1, ylab="(returns)^2")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
  acf(abs(sp500DailyReturns.mat), main="SP500 absolute returns")
  acf(sp500DailyReturns.mat^2, main="SP500 squared returns")
par(mfrow=c(1,1))
