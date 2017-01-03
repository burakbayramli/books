#  descriptiveStatistics.r
#
#  script file for computing descriptive statistics on financial returns
#  Examples are used in the chapter Descriptive Statistics for Financial Data
#
#	author: Eric Zivot
#	created: September 18, 2008
#
# To do
# 1. Change zoo objects to xts objects. More functions work with xts and xts has better
#    subsetting
# 2. Change data downloads from get.hist.quote() in tseries to getSymbols from quantmod
# 3. label each example and prepare for knitr integration


#	revision history: 
#
# February 2, 2015
#
# July 10, 2014
#   Combined analysis of daily and monthly data. focus on simple returns instead of cc returns.
#   rename data objects to be consistent with lower camel case
# July 15, 2013
#   updated examples for Summer 2013
# July 11, 2012
#   updated examples for Summer 2012
# July 14, 2011
#   updated example for Summer 2011
# October 20, 2009
#   updated examples for Fall 2009 class
# October 13, 2008
# 
#	Core R functions used:
#
#	acf				compute sample autocovariances or autocorrelations	
#	apply				apply function to rows or columns of matrix
#	args				determine agruments of a function
#	boxplot			compute boxplot
#	cbind				combine data objects vertically
#	class				determine class of object
#	colIds			get column names from object
#	cor				compute sample correlation matrix
#	density			compute smoothed histogram
# ecdf      compute empirical CDF
#	end				get end date of time series
#	help				invoke help system
#	hist				compute histogram
#	legend			add legend to graph
#	length			compute column length of matrix
#	library			load R package
#	mean				compute sample mean
#	names				show names of object
#	par				set graphics parameters
#	plot				generic plot function
#	points			add points to a graph
#	qqline			add line to qq-plot
#	qqnorm			qq-plot against normal distribution
#	qt				compute quantiles of student t distribution
#	rlnorm			generate random data from log-normal distribution
#	rt				generate random data from student t distribution
#	scale				standardize a vector of data
#	pnorm				compute normal CDF
#	seq				generate sequence of numbers
#	sort				sort data
#	start				get start date of time series
#	stdev				compute sample standard deviation
#	ts.plot			time series plot
#	var				compute sample variance or covariance matrix
#	?				invoke help system
#
# R packages used
# PerformanceAnalytics
#   skewness    compute sample skewness
#   kurtosis    compute excess kurtosis
# tseries			Time series and computational finance
#		get.hist.quote	load data from Yahoo!
#	zoo
#		plot.zoo		plot zoo object
# sn  
#   rsn       simulate from skew-normal distribution
#   dsn       compute pdf of skew-normal distribution

# set options
options(digits=4, width=70)


# load packages
library(car)
library(corrplot)
library(PerformanceAnalytics)
library("tseries")
library("zoo")
library(sn)


# set paths for loading and saving objects
# set globalPath to point to MFTSR directory on local computer
# globalPath = "C:/Users/ezivot.SOCIOLOGY/Dropbox/FinBook/"
globalPath = "C:/Users/ezivot/Dropbox/FinBook/"
setwd(globalPath)
loadPath = paste(globalPath, "Data/", sep="")
savePath = paste(globalPath, "GRAPHS/", sep="")
Rpath = paste(globalPath, "R/", sep="")

###############################################################################################
# Sub-Section Example Data
###############################################################################################

#
# get monthly and daily adjusted closing price data on MSFT and SP500 from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# May 2012. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird
#

##
## Ex. Getting daily and monthly adjusted closing price data from Yahoo! in R
##

##
## chunk
##
msftPrices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

sp500Prices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

msftDailyPrices = get.hist.quote(instrument="msft", start="1998-01-01",
                                 end="2012-05-31", quote="AdjClose",
                                 provider="yahoo", origin="1970-01-01",
                                 compression="d", retclass="zoo")
sp500DailyPrices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                                  end="2012-05-31", quote="AdjClose",
                                  provider="yahoo", origin="1970-01-01",
                                  compression="d", retclass="zoo")
class(msftPrices)
colnames(msftPrices)
start(msftPrices)
end(msftPrices)
head(msftPrices, n=3)
head(msftDailyPrices, n=3)

##
## chunk
##
# add column names 
# change date index class of monthly prices to yearmon
colnames(msftPrices) = colnames(msftDailyPrices) = "MSFT"
colnames(sp500Prices) = colnames(sp500DailyPrices) = "SP500"
index(msftPrices) = as.yearmon(index(msftPrices))
index(sp500Prices) = as.yearmon(index(sp500Prices))

##
## chunk
##
# create zoo object with both prices
msftSp500Prices = merge(msftPrices, sp500Prices)
msftSp500DailyPrices = merge(msftDailyPrices, sp500DailyPrices)
head(msftSp500Prices, n=3)
head(msftSp500DailyPrices, n=3)

##
## chunk
##
# compute simple returns
msftRetS = Return.calculate(msftPrices, method="simple")
msftDailyRetS = Return.calculate(msftDailyPrices, method="simple")
sp500RetS = Return.calculate(sp500Prices, method="simple")
sp500DailyRetS = Return.calculate(sp500DailyPrices, method="simple")
msftSp500RetS = Return.calculate(msftSp500Prices, method="simple")
msftSp500DailyRetS = Return.calculate(msftSp500DailyPrices, method="simple")

##
## chunk
##
# remove first NA observation
msftRetS = msftRetS[-1]
msftDailyRetS = msftDailyRetS[-1]
sp500RetS = sp500RetS[-1]
sp500DailyRetS = sp500DailyRetS[-1]
msftSp500RetS = msftSp500RetS[-1]
msftSp500DailyRetS = msftSp500DailyRetS[-1]

## 
## chunk
##
# compute cc returns 
msftRetC = log(1 + msftRetS)
sp500RetC = log(1 + sp500RetS)
msftSp500RetC = merge(msftRetC, sp500RetC)

##
## End example
##


###############################################################################################
# Sub-Section Time Plots
###############################################################################################

# look at help file for plot method for zoo objects
?plot.zoo

##
## Ex. Time plots of monthly prices and returns.
##

# plot individual prices in separate graphs
plot(msftPrices,main="Monthly closing price of MSFT",
     ylab="Price", lwd=2, col="blue")
plot(sp500Prices,main="Monthly closing price of SP500",
     ylab="Price", lwd=2, col="blue")

##
## chunk
##
# plot individual prices in two panel graph
# Figure
win.metafile(filename=paste(savePath, "figDSmonthlyPrices.emf", sep=""))
plot(msftSp500Prices, main="", lwd=2, col="blue")
dev.off()

##
## chunk
##
# put returns on the same plot in separate panels
# panel function for plot.zoo to add horizontal line at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
# Figure
win.metafile(filename=paste(savePath, "figDSmonthlyReturns.emf", sep=""))
plot(msftSp500RetS, main="", panel=my.panel, lwd=2, col="blue")
dev.off()

##
## End Example
##

##
## Ex. Time plots of monthly prices and returns.
##

##
## chunk
##
# put returns on same plot in one panel and add a horizontal line
# Figure
win.metafile(filename=paste(savePath, "figDSmonthlyReturns2.emf", sep=""))
plot(msftSp500RetS, plot.type="single", main="",
     col = c("red", "blue"), lty=c("dashed", "solid"), 
     lwd=2, ylab="Returns")
abline(h=0)
legend(x="bottomright", legend=colnames(msftSp500RetS), 
       lty=c("dashed", "solid"), lwd=2, 
       col=c("red","blue"))
dev.off()

##
## End Example
##


# use PerformanceAnalytics function chart.TimeSeries for nicer time series graphs
chart.TimeSeries(msftRetC)
chart.TimeSeries(MSFTsp500RetC)

# show two graphs
par(mfrow=c(2,1))
  chart.TimeSeries(msftRetC)
  chart.TimeSeries(sp500RetC)
par(mfrow=c(1,1))

# use layout for two graphs - same as above
layout(matrix(c(1,2), 2, 1))
chart.TimeSeries(msftRetC)
chart.TimeSeries(sp500RetC)
layout(matrix(1, 1, 1))

##
## Ex. Comparing simple and continuously compounded returns
##

##
## chunk
##
retDiff = msftRetS - msftRetC
dataToPlot = merge(msftRetS, msftRetC, retDiff)
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
win.metafile(filename=paste(savePath, "figDScompareReturns.emf", sep=""))
plot(dataToPlot, plot.type="multiple", main="",
     panel = my.panel,
     lwd=2, col=c("black", "blue", "red"))
dev.off()

##
## End Example
##

#
# Ex. Plotting Daily Returns
#

##
## chunk
##
win.metafile(filename=paste(savePath, "figDSdailyReturns.emf", sep=""))
plot(msftSp500DailyRetS, main="",
     panel=my.panel, col=c("black", "blue"))
dev.off()

##
## End Example
##

##
## Ex. Equity curves for Microsoft and S&P 500 monthly returns
##

## 
## chunk
##

equityCurveMsft = cumprod(1 + msftRetS)  
equityCurveSp500 = cumprod(1 + sp500RetS)  
dataToPlot = merge(equityCurveMsft, equityCurveSp500)
# Figure
win.metafile(filename=paste(savePath, "figDSmonthlyCumulativeReturns.emf", sep=""))
plot(dataToPlot, plot.type="single", ylab="Cumulative Returns",
     col=c("black", "blue"), lwd=2)
legend(x="topright", legend=c("MSFT", "SP500"),
       col=c("black", "blue"), lwd=2)
dev.off()

##
## End Example
##

# use as end of chapter exercise
chart.CumReturns(msftSp500RetS, wealth.index = 1, main="",
                 colorset = c("black", "blue"),
                 legend.loc = "topright")

##
## Ex. Drawdowns
##

# to be completed

###############################################################################################
# Sub-Section: Descriptive Statistics for the Distribution of Returns
###############################################################################################

#
# Sub-sub-section: histograms

# use hist() command

args(hist)
?hist
hist(msftRetS, main="Histogram of MSFT monthly returns", 
     col="cornflowerblue")
# scale histogram so that total area = 1
hist(msftRetS, main="Histogram of MSFT monthly returns",
     probability=TRUE, col="cornflowerblue")
# histogram of S&P 500 data
hist(sp500RetS, main="Histogram of SP500 monthly returns", 
     col="cornflowerblue")

##
## Ex. Histograms for the daily and monthly returns on Microsoft and the S&P 500 index
##

##
## chunk
##

# plot both histograms on same graph
# note different scales
# Figure
win.metafile(filename=paste(savePath, "figDSmsftsp500histograms.emf", sep=""))
par(mfrow=c(2,2))
  hist(msftRetS, main="", col="cornflowerblue")
  hist(msftDailyRetS, main="", col="cornflowerblue")
  hist(sp500RetS, main="", col="cornflowerblue")
  hist(sp500DailyRetS, main="", col="cornflowerblue")
par(mfrow=c(1,1))
dev.off()


##
## chunk
##

# use same breakpoints for both histograms
msftHist = hist(msftRetS, plot=FALSE, breaks=15)
class(msftHist)
names(msftHist)

# Figure
win.metafile(filename=paste(savePath, "figDSmsftsp500histograms2.emf", sep=""))
par(mfrow=c(2,2))
  hist(msftRetS, main="", col="cornflowerblue")
  hist(msftDailyRetS, main="", col="cornflowerblue",
       breaks=msftHist$breaks)
  hist(sp500RetS, main="", col="cornflowerblue", 
       breaks=msftHist$breaks)
  hist(sp500DailyRetS, main="", col="cornflowerblue",
       breaks=msftHist$breaks)
par(mfrow=c(1,1))
dev.off()

##
## End Example
##

# Use PerformanceAnalytics function chart.Histogram
chart.Histogram(msftRetC, colorset = "cornflowerblue")


##
## Ex. Are Microsoft returns normally distributed? A first look.
##

##
## chunk
##
# create simulated iid Gaussian data with same mean and SD
# as MSFT for both monthly and daily simple returns
set.seed(123)
gwnDaily = rnorm(length(msftDailyRetS), mean=mean(msftDailyRetS),
                 sd=sd(msftDailyRetS))
gwnDaily = zoo(gwnDaily, index(msftDailyRetS))
gwnMonthly = rnorm(length(msftRetS), mean=mean(msftRetS),
                   sd=sd(msftRetS))
gwnMonthly = zoo(gwnMonthly, index(msftRetS))

# compare gwn to monthly returns
par(mfrow=c(2,1))
  plot(msftRetS,main="Monthly returns on MSFT", lwd=2, col="blue")
  abline(h=0)
  ts.plot(gwnMonthly, main="GWN with same mean and sd as MSFT", 
          lwd=2, col="blue")
  abline(h=0)
par(mfrow=c(1,1))

# shows time plots and histograms together for Microsoft monthly returns and GWN
# could also use type="h

##
## chunk
##
win.metafile(filename=paste(savePath, "figDScompareMSFTnormal.emf", sep=""))
par(mfrow=c(2,2))
  plot(msftRetS, main="Monthly Returns on MSFT", 
       lwd=2, col="blue", ylim=c(-0.4, 0.4))
  abline(h=0)
  plot(gwnMonthly, main="Simulated Normal Returns",  
       lwd=2, col="blue", ylim=c(-0.4, 0.4))
  abline(h=0)
  hist(msftRetS, main="", col="cornflowerblue", 
       xlab="returns")
  hist(gwnMonthly, main="", col="cornflowerblue", 
       xlab="returns", breaks=msftHist$breaks)
par(mfrow=c(1,1))
dev.off()

# shows time plots and histograms together for Microsoft monthly returns and GWN
# could also use type="h

##
## chunk
##
msftDailyHist = hist(msftDailyRetS, plot=FALSE, breaks=15)
win.metafile(filename=paste(savePath, "figDScompareMSFTnormalDaily.emf", sep=""))
par(mfrow=c(2,2))
  plot(msftDailyRetS, main="Monthly Returns on MSFT", 
       lwd=2, col="blue", ylim=c(-0.15, 0.15))
  abline(h=0)
  plot(gwnDaily, main="Simulated Normal Returns",  
       lwd=2, col="blue", ylim=c(-0.15, 0.15))
  abline(h=0)
  hist(msftDailyRetS, main="", col="cornflowerblue", 
       xlab="returns")
  hist(gwnDaily, main="", col="cornflowerblue", 
       xlab="returns", breaks=msftDailyHist$breaks)
par(mfrow=c(1,1))
dev.off()

##
## End Example
##

#
# sub-sub-section smoothed histograms
#

# use density() command
?density
args(density)

##
## Ex. Smoothed histogram for Microsoft monthly returns
##

##
## chunk
##
MSFT.density = density(msftRetS)
# put histogram and density plot on same graph
# Figure
win.metafile(filename=paste(savePath, "figDSsmoothedHistogramMsft.emf", sep=""))
hist(msftRetS, main="", xlab="Microsoft Monthly Returns", 
     col="cornflowerblue", probability=T, ylim=c(0,5))
points(MSFT.density,type="l", col="orange", lwd=2)
dev.off()

##
## End Example
##

# look at density object
class(MSFT.density)
names(MSFT.density)
MSFT.density

# do the same for s&p 500
SP500.density = density(sp500RetS)
plot(SP500.density,type="l",xlab="Monthly return", col="orange", lwd=2,
     ylab="density estimate",main="Smoothed histogram: SP500")

# combine density plots on one graph
hist(sp500RetS, main="Histogram and smoothed density",
     probability=T, ylim=c(0,10), col="cornflowerblue")
points(SP500.density,type="l", lwd=2, col="orange")


#
# sub-sub-section Empirical CDF
#

# compute and plot empirical distribution function for simulated gaussian data
n1 = length(gwnMonthly)
plot(sort(coredata(gwnMonthly)),(1:n1)/n1,type="s",ylim=c(0,1), col="cornflowerblue", lwd=2,
     main="Empirical CDF of Gaussian data", ylab="#x(i) <= x")

# compare empirical cdf to standard normal cdf for simulated gaussian data
z1 = scale(coredata(gwnMonthly))  		# standardize to have mean zero and sd 1
n1 = length(gwnMonthly)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

# The following plot options are used
# type		determine type of plot: "l" is line plot; "s" is step plot
# lty		line type: 1 is solid line; 3 is dot-dashed line
# lwd		line thickness: higher values give thicker lines
# col		line color: 1 is black, 2 is blue etc. 
# For help on plot options, see help(par)

plot(x1,y1,main="Empirical CDF vs. Normal CDF for Gaussian data",
     type="l",lwd=2,xlab="standardized gwn",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=2, col=c("black","orange"))

# compare empirical cdf to standard normal cdf for MSFT returns
z1 = scale(coredata(msftRetS))			# standardize to have mean zero and sd 1
n1 = length(msftRetS)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

plot(x1,y1,main="Empirical CDF vs. Normal CDF for MSFT returns",
     type="l",lwd=2,xlab="standardized MSFT returns",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=c(2,3), col=c("black","orange"))

# compare empirical cdf to standard normal cdf for SP500 returns
z1 = scale(coredata(sp500RetS))			# standardize to have mean zero and sd 1
n1 = length(sp500RetS)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

plot(x1,y1,main="Empirical CDF vs. Normal CDF for SP500 returns",
     type="l",lwd=2,xlab="standardized SP500 returns",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=c(2,3), col=c("black","orange"))


#
# sub-sub-section Empirical quantiles/percentiles
#


# use quantile() function
?quantile
args(quantile)

##
## Ex. Empirical quantiles of the Microsoft and S&P 500 monthly returns
##

## 
## chunk
##
# empirical quantiles for MSFT
quantile(msftRetS)
quantile(sp500RetS)

##
## chunk
##
# 1% and 5% quantiles
quantile(msftRetS,probs=c(0.01,0.05))
quantile(sp500RetS,probs=c(0.01,0.05))

##
## chunk
##
# median and IQR
apply(msftSp500RetS, 2, median)
apply(msftSp500RetS, 2, IQR)

##
## End Example
##

# compare to normal quantiles
qnorm(p=c(0.01,0.05), mean=mean(msftRetS), 
      sd=sd(msftRetS))
# empirical and normal quantiles for SP500
quantile(sp500RetS,probs=c(0.01,0.05))
qnorm(p=c(0.01,0.05), mean=mean(sp500RetS), 
      sd=sd(sp500RetS))

#
# sub-sub-section Historical VaR
#

# monthly historical VaR - $100,000 investment
q.01 = quantile(msftRetS, probs=0.01)
q.05 = quantile(msftRetS, probs=0.05)
q.01
q.05
VaR.01 = 100000*q.01
VaR.05 = 100000*q.05
VaR.01
VaR.05

#
# Ex. Using empirical quantiles to compute historical Value-at-Risk
#

##
## chunk
##
W = 100000
msftQuantiles = quantile(msftRetS, probs=c(0.01, 0.05))
sp500Quantiles = quantile(sp500RetS, probs=c(0.01, 0.05))
msftVaR = W*msftQuantiles
sp500VaR = W*sp500Quantiles
msftVaR
sp500VaR

#
# sub-section: QQ-plots
#

#
# Ex. Normal QQ-plots for GWN, Microsoft and S&P 500 returns
#

##
## chunk
##
# Figure
win.metafile(filename=paste(savePath, "figDSqqplots.emf", sep=""))
par(mfrow=c(2,3))	# 4 panel layout: 2 rows and 2 columns
	qqnorm(gwnMonthly, main="GWN Monthly", col="cornflowerblue")
	qqline(gwnMonthly)
	qqnorm(msftRetS, main="MSFT Monthly Returns", col="cornflowerblue")
	qqline(msftRetS)
	qqnorm(sp500RetS, main="SP500 Monthly Returns", col="cornflowerblue")
	qqline(sp500RetS)
  qqnorm(gwnDaily, main="GWN Daily", col="cornflowerblue")
  qqline(gwnDaily)
  qqnorm(msftDailyRetS, main="MSFT Daily Returns", col="cornflowerblue")
  qqline(msftDailyRetS)
  qqnorm(sp500DailyRetS, main="SP500 Daily Returns", col="cornflowerblue")
  qqline(sp500DailyRetS)
par(mfrow=c(1,1))
dev.off()

##
## End example
##

# compare normal qq plots for simple and cc returns
par(mfrow=c(2,2))
  qqnorm(msftRetS, main="MSFT Monthly Returns", col="cornflowerblue")
  qqline(msftRetS)
  qqnorm(msftRetC, main="MSFT Monthly CC Returns", col="cornflowerblue")
  qqline(msftRetC)
  qqnorm(sp500RetS, main="SP500 Monthly Returns", col="cornflowerblue")
  qqline(sp500RetS)
  qqnorm(sp500RetS, main="SP500 Monthly CC Returns", col="cornflowerblue")
  qqline(sp500RetS)
par(mfrow=c(1,1))

# Data for student t with 3 df: tails fatter than normal
set.seed(123)
tdata = rt(200,df=3)		# Student-t with 3 df
gdata = rnorm(200)			# N(0,1) data
xx = seq(from=-5,to=5,length=100)

# data for log-normal: asymmetric distribution
lndata = rlnorm(200)
yy = seq(from=-3, to = 3, length=100)

# data for skew normal: asymmetric distribution
set.seed(223)
rightSkewData = rsn(200, alpha = 2)
leftSkewData = rsn(200, alpha = -2)

#
# homework problem, illustrate QQ plots for fat tailed and skewed distributions
#
par(mfrow=c(2,3))
	# 1st plot
	plot(xx,dnorm(xx),type="l", lwd=2,
	     main="Normal and Student-t with 3 df", xlab = "z, t", ylab = "pdf", col="cornflowerblue")
	points(xx,dt(xx,df=3), type="l", col="orange", lwd=3)
	legend(x="topright", legend=c("Normal","Student-t"), lty=c(1,1), col=c("cornflowerblue","orange"),
	       lwd=c(2,3))
	# 2nd plot
	plot(yy,dnorm(yy,sd=1),type="l", lwd=2, ylim=c(0,0.7),
	     main="Normal and Skew-Normal", xlab = "z, skew-z", ylab = "pdf",
	     col="cornflowerblue")
	points(yy,dsn(yy, alpha=2), type="l", lwd=3, col="orange")
	legend(x="topleft", legend=c("Normal","Skew-Normal"), lty=c(1,1), col=c("cornflowerblue","orange"),
	       lwd=c(2,3))
  # 3rd plot
  plot(yy,dnorm(yy,sd=1),type="l", lwd=2, ylim=c(0,0.7),
       main="Normal and Skew-Normal", xlab = "z, skew-z", ylab = "pdf", col="cornflowerblue")
  points(yy,dsn(yy, alpha=-2), type="l", lwd=3, col="orange")
  legend(x="topright", legend=c("Normal","Skew-Normal"), lty=c(1,1), col=c("cornflowerblue","orange"),
         lwd=c(2,3))
	# 4th plot
	qqnorm(tdata, col="cornflowerblue")
	qqline(tdata)
	# 5th plot
	qqnorm(rightSkewData, col="cornflowerblue")
	qqline(rightSkewData)
  # 6th plot
  qqnorm(leftSkewData, col="cornflowerblue")
  qqline(leftSkewData)
par(mfrow=c(1,1))


#
# Ex. Student's t QQ-plot for Microsoft returns
#

# create QQ-plot with Student's t distribution with 5 df for MSFT returns
##
## chunk
##
library(car)
win.metafile(filename=paste(savePath, "figDSqqplotStudent.emf", sep=""))
qqPlot(coredata(msftRetS), distribution="t", df=5, 
       ylab="MSFT quantiles", envelope=FALSE)
dev.off()

##
## End example
##

#
# Sub-section Shape Characteristics of the Empirical Distribution
#

# use mean, var, sd, skewness and kurtosis functions
mean(msftRetS)
var(msftRetS)
sd(msftRetS)
skewness(msftRetS)
kurtosis(msftRetS)
# kurtosis function actually computes excess kurtosis
kurtosis(msftRetC) + 3
# note: summary is a generic function with several methods
summary(msftRetS)

# Use apply to compute statistics for columns
mean.vals = apply(msftSp500RetS, 2, mean)
var.vals = apply(msftSp500RetS, 2, var)
sd.vals = apply(msftSp500RetS, 2, sd)
skew.vals = apply(msftSp500RetS, 2, skewness)
kurt.vals = apply(msftSp500RetS, 2, kurtosis)
stats.mat = rbind(mean.vals,var.vals, sd.vals, 
                  skew.vals, kurt.vals)

#
# Ex. Sample shape statistics for the returns on Microsoft and S&P 500
# 

# Monthly stats
##
## chunk
##
statsMat = rbind(apply(msftSp500RetS, 2, mean),
                  apply(msftSp500RetS, 2, var),
                  apply(msftSp500RetS, 2, sd),
                  apply(msftSp500RetS, 2, skewness),
                  apply(msftSp500RetS, 2, kurtosis))
rownames(statsMat) = c("Mean", "Variance", "Std Dev",
                       "Skewness",  "Excess Kurtosis")
round(statsMat, digits=4)

# Daily stats
##
## chunk
##
statsMatDaily = rbind(apply(msftSp500DailyRetS, 2, mean),
                      apply(msftSp500DailyRetS, 2, var),
                      apply(msftSp500DailyRetS, 2, sd),
                      apply(msftSp500DailyRetS, 2, skewness),
                      apply(msftSp500DailyRetS, 2, kurtosis))
rownames(statsMatDaily) = rownames(statsMat)
round(statsMatDaily, digits=4)

# check square root of time rule
##
## chunk
##
12*statsMatDaily["Mean", ]
sqrt(12)*statsMatDaily["Std Dev", ]

##
## End example
##


# 
# Sub-section outliers
#

# create GWN return data polluted by outlier
gwnMonthlyOutlier = gwnMonthly
gwnMonthlyOutlier[20] = -sd(gwnMonthly)*6

# Figure
win.metafile(filename=paste(savePath, "figDSgwnOutlier.emf", sep=""))
par(mfrow=c(2,1))
	plot(gwnMonthlyOutlier,main="", lwd=2, col="blue", ylab="")
	abline(h=0)
	hist(gwnMonthlyOutlier, main="", col="cornflowerblue", 
       xlab="GWN with outlier")
par(mfrow=c(1,1))
dev.off()

# compare summary statistic for MSFT returns and returns polluted by outlier
gwnMonthlyBoth = cbind(gwnMonthly, gwnMonthlyOutlier)
statsMat = rbind(apply(gwnMonthlyBoth, 2, mean),
                 apply(gwnMonthlyBoth, 2, var),
                 apply(gwnMonthlyBoth, 2, sd),
                 apply(gwnMonthlyBoth, 2, skewness),
                 apply(gwnMonthlyBoth, 2, kurtosis),
                 apply(gwnMonthlyBoth, 2, median),
                 apply(gwnMonthlyBoth, 2, IQR))
rownames(statsMat) = c("Mean", "Var", "SD", "skewness", "kurtosis",
                       "median", "IQR")
statsMat["kurtosis", ] = statsMat["kurtosis", ] + 3
pctchange = (statsMat[, 2] - statsMat[, 1])/statsMat[, 1]
statsMat = cbind(statsMat, pctchange)
round(statsMat, digits=4)
#
# 12. boxplots
#
# use boxplot() function
# note: boxplot() works on xts objects but not zoo objects!!!!
?boxplot
args(boxplot)
boxplot(msftRetS,outchar=T,main="Boxplot of monthly cc returns on Microsoft",
        ylab="monthly cc return", col="cornflowerblue")
boxplot(sp500RetC,outchar=T,main="Boxplot of monthly cc returns on SP500",
        ylab="monthly cc return")
boxplot(coredata(gwnMonthly), coredata(msftRetS), coredata(sp500RetS),
        names=c("gwn","MSFT","SP500"),
        outchar=T, col="cornflowerblue",
        main="Comparison of return distributions", 
        ylab="monthly returns")

##
## EX. Boxplots of return distributions
##

##
## chunk
##
win.metafile(filename=paste(savePath, "figDSboxplotReturns.emf", sep=""))
boxplot(coredata(msftRetS), coredata(msftRetC), 
        coredata(sp500RetS), coredata(sp500RetC),
        names=c("msftRetS", "msftRetC", 
                "sp500RetS", "sp500RetC"),
        col="cornflowerblue")
dev.off()

dataToPlot = merge(msftRetS,msftRetC,sp500RetS,sp500RetC)
colnames(dataToPlot) = c("msftRetS", "msftRetC", 
                         "sp500RetS", "sp500RetC")
chart.Boxplot(dataToPlot)

#
# 13. graphically summarize data (see Carmona book)
#

par(mfrow=c(2,2))
	hist(msftRetS,main="MSFT monthly cc returns",
	     probability=T, ylab="cc return", col="cornflowerblue")
	boxplot(msftRetS,outchar=T, ylab="cc return", col="cornflowerblue")
	plot(MSFT.density,type="l",xlab="cc return", col="cornflowerblue", lwd=2,
	     ylab="density estimate", main="Smoothed density")
	qqnorm(msftRetS)
	qqline(msftRetS)
par(mfrow=c(1,1))


# example of four panel plot
fourPanelPlot = function(ret) {
  # ret     n.dates x 1 matrix of returns. It is assumed that the 
  #         column has a name 
  retName = colnames(ret)
  ret.den = density(ret)
  par(mfrow=c(2,2))
  hist(ret, main=paste(retName, " monthly returns", sep=""),
       xlab=retName, probability=T, col="cornflowerblue")
  boxplot(ret, outchar=T,col="cornflowerblue")
  plot(ret.den, main="smoothed density", 
       type="l", lwd=2,
       xlab="monthly return",
       ylab="density estimate")
  # overlay normal distribution on smoothed density
  lines(ret.den$x, dnorm(ret.den$x, mean=mean(ret), sd=sd(ret)),
        col="cornflowerblue", lwd=2)
  legend(x="topleft", legend=c("smoothed", "normal"),
         lty=c(1,1), col=c("black", "blue"), lwd=2)
  qqnorm(ret, col="cornflowerblue", pch=16)
  qqline(ret)
  par(mfrow=c(1,1))
}

fourPanelPlot(coredata(msftRetC))
#
# 15. Time series descriptive statistics
#

# sample autocovariances and autocorrelations

?acf
args(acf)
# autocorrelations for simulated gaussian data and SP500
# be careful with zoo objects!!!!

# compute autocorrelations
acf(coredata(msftRetS), lag.max=5, plot=FALSE)
acf(coredata(msftDailyRetS), lag.max=5, plot=FALSE)

win.metafile(filename=paste(savePath, "figDSsacf.emf", sep=""))
par(mfrow=c(2,2))
  acf(coredata(msftRetS), main="msftRetS", lwd=2)
  acf(coredata(sp500RetS), main="sp500RetS", lwd=2)
  acf(coredata(msftDailyRetS), main="msftDailyRetS", lwd=2)
  acf(coredata(sp500DailyRetS), main="sp500DailyRetS", lwd=2)
par(mfrow=c(1,1))
dev.off()


#
# nonlinear time dependence
#
n.obs = 500
set.seed(123)
gwn = rnorm(n.obs)

win.metafile(filename=paste(savePath, "figDSgwnIndep.emf", sep=""))
par(mfrow=c(3, 2))
  ts.plot(gwn)
  abline(h=0)
  acf(gwn, lwd=2)
  ts.plot(gwn^2)
  acf(gwn^2, lwd=2)
  ts.plot(abs(gwn))
  acf(abs(gwn), lwd=2)
par(mfrow=c(1,1))
dev.off()


# Let's see if MSFT returns are independent - look at time dependence in abs() and
# squares.

# plot daily returns, absolute returns and squared returns

dataToPlot = merge(msftDailyRetS, abs(msftDailyRetS), msftDailyRetS^2)
colnames(dataToPlot) = c("Returns", "abs(Returns)", "Returns^2")
plot(dataToPlot, main="Daily Returns", col="blue")


# plot monthly returns, absolute returns and squared returns
dataToPlot = merge(msftRetS, abs(msftRetS), msftRetS^2)
colnames(dataToPlot) = c("Returns", "abs(Returns)", "Returns^2")
plot(dataToPlot, main="monthly Returns", col="blue")

# do the same for the s&p 500 index
# plot daily returns, absolute returns and squared returns

##
## Ex. Nonlinear time dependence in the S&P 500 daily returns
##
dataToPlot = merge(sp500DailyRetS, abs(sp500DailyRetS), sp500DailyRetS^2)
colnames(dataToPlot) = c("Returns", "abs(Returns)", "Returns^2")
# Figure
win.metafile(filename=paste(savePath, "figDSsp500vol.emf", sep=""))
plot(dataToPlot, main="Daily Returns", col="blue")
dev.off()

# Figure
win.metafile(filename=paste(savePath, "figDSsp500vol2.emf", sep=""))
par(mfrow=c(2,2))
acf(abs(coredata(sp500DailyRetS)), main="Daily Absolute Returns", lwd=2)
acf(coredata(sp500DailyRetS)^2, main="Daily Squared Returns", lwd=2)
acf(abs(coredata(sp500RetS)), main="Monthly Absolute Returns", lwd=2)
acf(coredata(sp500RetS)^2, main="Monthly Squared Returns", lwd=2)
par(mfrow=c(1,1))
dev.off()


# plot monthly returns, absolute returns and squared returns
dataToPlot = merge(sp500RetS, abs(sp500RetS), sp500RetS^2)
colnames(dataToPlot) = c("Returns", "abs(Returns)", "Returns^2")
plot(dataToPlot, main="monthly Returns", col="blue")


par(mfrow=c(2,2))
  acf(abs(coredata(msftDailyRetS)), main="Daily Absolute Returns", lwd=2)
  acf(coredata(msftDailyRetS)^2, main="Daily Squared Returns", lwd=2)
  acf(abs(coredata(msftRetS)), main="Monthly Absolute Returns", lwd=2)
  acf(coredata(msftRetS)^2, main="Monthly Squared Returns", lwd=2)
par(mfrow=c(1,1))





#
# 14. bivariate graphical summaries
#

##
## Ex. Scatterplot of Microsoft and S&P 500 returns
##

##
## chunk
##
win.metafile(filename=paste(savePath, "figDSscatterplot.emf", sep=""))
# bivariate scatterplot
plot(sp500RetS,msftRetS, 
     main="Monthly cc returns on MSFT and SP500", 
     xlab="S&P500 returns", ylab="MSFT returns",
     lwd=2, pch=16, cex=1.25, col="blue")
abline(v=mean(sp500RetS))  
abline(h=mean(msftRetS))	
dev.off()

##
## end example
##

##
## Ex. Pair-wise scatterplots for multiple series
##

##
## chunk
##

# all pairwise scatterplots
win.metafile(filename=paste(savePath, "figDSpairs.emf", sep=""))
pairs(cbind(gwnMonthly,msftRetS,sp500RetS), col="blue",
      pch=16, cex=1.25, cex.axis=1.25)
dev.off()

##
## end example
##

##
## Ex. Sample covariance and correlation between Microsoft and S&P 500 returns
##

##
## chunk
##
# pairwise covariances and correlations
cov(sp500RetS, msftRetS)
cor(sp500RetS, msftRetS)

##
## chunk
#
# compute covariance and correlation matrix
cov(msftSp500RetS)
cor(msftSp500RetS)

##
## chunk
#
cov2cor(cov(msftSp500RetS))
##
## end example
##

##
## Ex. visualizing correlation matrices
##

##
## chunk
##

dataToPlot = merge(gwnMonthly, msftRetS, sp500RetS)
cor.mat = cor(dataToPlot)
win.metafile(filename=paste(savePath, "figDScorrplot.emf", sep=""))
corrplot.mixed(cor.mat, lower="number", upper="ellipse")
dev.off()

