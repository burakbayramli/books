# 424project2010.r		script file for econ 424 Project calculations
#
# author: Eric Zivot
# created: November 9, 2009
# revision history
# October 22, 2010
#   Update script for Fall 2010. Data is now automatically loaded using get.hist.quote()
#	December 1, 2009
#		Added VaR and bootstrap VaR code
#
# Data for the project are downloaded automatically from Yahoo! and consist of
# closing price data on 6 Vanguard mutual funds:
#
# 1. S&P 500 index (vfinx)
# 2. European stock index (veurx)
# 3. Emerging markets fund (veiex)
# 4. Long term bond index (vbltx)
# 5. Short term bond index (vbisx)
# 6. Pacific stock index (vpacx)

# Some of the analysis will use the the portfolio functions I wrote in the file
# portfolio.r on the class syllabus page. Download this file to your computer. You will
# source() in this file later

options(digits=4, width=70)
# load packages
library("PerformanceAnalytics")
library("tseries")
library("zoo")
library("boot")
# change this to the appropriate path on your computer
savePath="C:/Users/ezivot/Documents/classes/econ424/fall2009/project/"

# source portfolio functions
# change the path to the appropriate path on your computer
loadPath = "C:\\Users\\ezivot\\Documents\\FinBook\\R\\"
source(file=paste(loadPath, "portfolio.r", sep=""))

#
# 1. load data from Yahoo!
#

# get monthly adjusted closing price data on Vanguard mutual fund data from Yahoo
# using the tseries function get.hist.quote. Set sample to Sept 2005 through
# Sept 2010. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")

vfinx.prices = get.hist.quote(instrument="vfinx", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
veurx.prices = get.hist.quote(instrument="veurx", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
veiex.prices = get.hist.quote(instrument="veiex", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbltx.prices = get.hist.quote(instrument="vbltx", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbisx.prices = get.hist.quote(instrument="vbisx", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start="2005-09-01",
                             end="2010-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
# change time indices to class yearmon, which is most appropriate for monthly data
index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbltx.prices) = as.yearmon(index(vbltx.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names

#
# 2. compute cc returns
#

projectReturns.z = diff(log(projectPrices.z))         

#
# 3. plot data
#
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(projectPrices.z, col="blue", lwd=2)
plot(projectReturns.z, panel=my.panel, col="blue", lwd=2)
# plot growth of $1 over the five years using PerformanceAnalytics function
# chart.CumReturns
projectReturnsSimple.z = exp(projectReturns.z) - 1
chart.CumReturns(projectReturnsSimple.z, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="growth of $1") 

#
# 4. Create matrix of return data and compute pairwise scatterplots
#

ret.mat = coredata(projectReturns.z)
pairs(ret.mat, col="blue")

# example of four panel plot
par(mfrow=c(2,2))
	hist(ret.mat[,"vfinx"],main="vfinx monthly returns",
	     xlab="vfinx", probability=T, col="slateblue1")
	boxplot(ret.mat[,"vfinx"],outchar=T,col="slateblue1")
	plot(density(ret.mat[,"vfinx"]), main="smoothed density", 
       type="l",xlab="monthly return",
	     ylab="density estimate")
	qqnorm(ret.mat[,"vfinx"])
	qqline(ret.mat[,"vfinx"])
par(mfrow=c(1,1))


#
# compute descriptive statistics
#

muhat.vals = colMeans(projectReturns.z)
sd.vals = apply(projectReturns.z, 2, sd)
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
# empirical quantiles for VaR calculations
q.vals = apply(projectReturns.z, 2, quantile, prob=c(0.01,0.05))

## write covariance matrix, expected returns, sd values and quantiles to files
## for importing into Excel

write.csv(cov.mat, file=paste(savePath, "covmat.csv", sep=""))
write.csv(muhat.vals, file=paste(savePath, "muhat.csv", sep=""))
write.csv(sd.vals, file=paste(savePath, "sd.csv", sep=""))
write.csv(t(q.vals), file=paste(savePath, "q.csv", sep=""))


#
# VaR analysis
#

# function to compute normal VaR for a matrix of returns

Value.at.Risk = function(x,p=0.05,w=100000) {
	x = as.matrix(x)
	q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
	VaR = (exp(q) - 1)*w
	VaR
}

# compute 5% and 1% normal VaR for all assets
VaR.05 = Value.at.Risk(ret.mat)
VaR.05
VaR.01 = Value.at.Risk(ret.mat, p=0.01)
VaR.01

# function to bootstrap VaR
ValueAtRisk.boot = function(x, idx, p=0.05, w=100000) {

	q = mean(x[idx]) + sd(x[idx])*qnorm(p)
	VaR = (exp(q) - 1)*w
	VaR
}

# bootstrap vfinx
VaR.05.boot.vfinx = boot(ret.mat[, "vfinx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vfinx
boot.ci(VaR.05.boot.vfinx, conf = 0.95, type = c("norm","perc"))
plot(VaR.05.boot.vfinx)


## risk free rate
rf = 0.005/12

#
# Portfolio theory allowing for short sales
#

## compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=rf)
plot(gmin.port)

## efficient portfolio with target return equal to max returns
target.return <- max(muhat.vals)
e.port.max<- efficient.portfolio(muhat.vals, cov.mat, target.return)
e.port.max
summary(e.port.max, risk.free=rf)
plot(e.port.max)

## compute tangency portfolio
tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
tan.port
summary(tan.port, risk.free=rf)
plot(tan.port)

## compute efficient frontier
ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, 
                         alpha.max=1.5, nport=20)

## plot efficient frontier
plot(ef, plot.assets=T, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port$sd, tan.port$er, col="red", lwd=2)
sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)

#
# singleIndex Model
# 

veurx.fit = lm(veurx ~ vfinx, data=projectReturns.z)
summary(veurx.fit)

veiex.fit = lm(veiex ~ vfinx, data=projectReturns.z)
summary(veiex.fit)

vbltx.fit = lm(vbltx ~ vfinx, data=projectReturns.z)
summary(vbltx.fit)

vbisx.fit = lm(vbisx ~ vfinx, data=projectReturns.z)
summary(vbisx.fit)

vpacx.fit = lm(vpacx ~ vfinx, data=projectReturns.z)
summary(vpacx.fit)

beta.vals = c(1,
              coef(veurx.fit)[2], 
              coef(veiex.fit)[2],
              coef(vbltx.fit)[2],
              coef(vbisx.fit)[2],
              coef(vpacx.fit)[2])
names(beta.vals) = colnames(projectReturns.z)
write.csv(beta.vals, file=paste(savePath, "beta.csv", sep=""))              

