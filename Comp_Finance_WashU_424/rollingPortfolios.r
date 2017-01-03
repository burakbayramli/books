# rollingPortfolios.r				Examples of rolling analysis of portfolios
#
# author: Eric Zivot
# created: November 26, 2006
# update history: 
# August 21, 2013
#   Updated for Summer 2013
# November 16, 2008
#
# notes:
# 1. requires portfolio functions in portfolio.r
# 2. data are imported from singleIndexPrices.csv 
# which contains monthly closing prices on the following assets
#
# sp500, sbux, msft, nord, boeing
#
# over the period 10/98 - 10/03
#
library(zoo)
library(PerformanceAnalytics)
options(digits=4)      
cex.val = 1.5
computer = "laptop"

# load portfolio theory functions
# source portfolio functions
source(file="http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r")


if (computer == "office") {
  loadPath = "C:\\Users\\ezivot.SOCIOLOGY\\Dropbox\\FinBook\\EXCEL\\" 
}
if (computer == "laptop") {
  loadPath = "C:\\Users\\ezivot\\Dropbox\\FinBook\\EXCEL\\"
}


singleIndexPrices.df = read.csv(paste(loadPath, "singleIndexPrices.csv", sep=""),
                                stringsAsFactors=F)
colnames(singleIndexPrices.df)

td = seq(as.Date("1998-01-01"), as.Date("2003-01-01"), by="months")
singleIndexPrices.z = zoo(singleIndexPrices.df[,-1], td)

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}                              
plot(singleIndexPrices.z, main="", lwd=2, col="blue")
                           
#
# 3. create returns
#
si.z = Return.calculate(singleIndexPrices.z, method="discrete")
si.z = si.z[-1,]
si.df = as.data.frame(si.z)
head(si.df)

# returns excluding market
ret.z = si.z[, -1]
ret.mat = as.matrix(si.df[,-1])
n.obs = nrow(ret.mat)

# plot returns over full sample
plot(ret.z, main="", plot.type="single",ylab="returns",
     cex.axis = 1.5, cex.lab = 1.5, lwd=2, col=1:4)
abline(h=0)
legend(x="bottomright", legend=colnames(ret.z),
       col=1:4, lwd=2, cex=1.5)

pairs(ret.mat, col="slateblue1", pch=16, cex=1.5)


# estimate parameters of constant expected return
# model


nobs = nrow(ret.mat)
muhat.vals = colMeans(ret.mat)
sigmahat.vals = apply(ret.mat,2,sd)
cov.mat = var(ret.mat)
cor.mat = cor(ret.mat)

se.muhat = sigmahat.vals/sqrt(nobs)
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat


# show risk return tradeoffs
plot(sigmahat.vals, muhat.vals,  ylim=c(0, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sigmahat.vals, muhat.vals, labels=names(muhat.vals), pos=4, cex = cex.val)

#
# Rolling means, SDs and Correlations
#

# compute rolling means and standard deviations over 24 month windows
roll.muhat.sbux = rollapply(ret.z[,"sbux"], width=24,
                       FUN=mean, align="right")
roll.sigmahat.sbux = rollapply(ret.z[,"sbux"],width=24,
                          FUN=sd, align="right")
roll.muhat.msft = rollapply(ret.z[,"msft"], width=24,
                            FUN=mean, align="right")
roll.sigmahat.msft = rollapply(ret.z[,"msft"],width=24,
                               FUN=sd, align="right")
roll.muhat.nord = rollapply(ret.z[,"nord"], width=24,
                            FUN=mean, align="right")
roll.sigmahat.nord = rollapply(ret.z[,"nord"],width=24,
                               FUN=sd, align="right")
roll.muhat.boeing = rollapply(ret.z[,"boeing"], width=24,
                            FUN=mean, align="right")
roll.sigmahat.boeing = rollapply(ret.z[,"boeing"],width=24,
                               FUN=sd, align="right")
# plot rolling estimates with data

par(mfrow=c(2,2))
# sbux
  plot(merge(roll.muhat.sbux,roll.sigmahat.sbux, ret.z[,"sbux"]), plot.type="single",
       main="SBUX",ylab="returns",
       lwd=2, col=c("blue","orange","black"))
  abline(h=0)

 legend(x="bottomright",legend=c("Rolling mean","Rolling sd", "Monthly returns"),
        lwd=2, col=c("blue","orange","black"))
# msft
  plot(merge(roll.muhat.msft,roll.sigmahat.msft, ret.z[,"msft"]), plot.type="single",
       main="MSFT",ylab="returns",
       lwd=2, col=c("blue","orange","black"))
  abline(h=0)         
# nord
  plot(merge(roll.muhat.nord,roll.sigmahat.nord, ret.z[,"nord"]), plot.type="single",
       main="NORD",ylab="returns",
       lwd=2, col=c("blue","orange","black"))
  abline(h=0)
# boeing
  plot(merge(roll.muhat.boeing,roll.sigmahat.boeing, ret.z[,"boeing"]), plot.type="single",
       main="SBUX",ylab="returns",
       lwd=2, col=c("blue","orange","black"))
  abline(h=0)
par(mfrow=c(1,1))

# legend(x="bottomleft",legend=c("Rolling mean","Rolling sd", "Monthly returns"),
#        lwd=2, col=c("blue","orange","black"))

# compute rolling standard deviations over 24 month windows

roll.cor = function(x) {
  cor.hat = cor(x)
  cor.vals = cor.hat[lower.tri(cor.hat)]
  names(cor.vals) = c("sbux.msft","sbux.nord","sbux.boeing",
                      "msft.nord","msft.boeing", "nord.boeing")
  return(cor.vals)
}
roll.cor.vals = rollapply(ret.z, width=24,
                          by.column=FALSE,
                          FUN=roll.cor, 
                          align="right")

# plot rolling correlations
plot(roll.cor.vals, panel=my.panel, main="",
     lwd=2, col="blue", ylim=c(-0.5,1))

#
# compute efficient portfolios
#

# global minimum variance portfolio
gmin.4 = globalMin.portfolio(er=muhat.vals,cov.mat=cov.mat)
gmin.4

# efficient portfolio with target return = 0.015
eport.015 = efficient.portfolio(er=muhat.vals,cov.mat=cov.mat,
                                target.return=0.015)
eport.015
plot(eport.015, col="slateblue1")

# compute efficient frontier
ef.4 = efficient.frontier(er=muhat.vals,cov.mat=cov.mat)
ef.4
plot(ef.4, plot.assets=TRUE)

#
# functions for computing rolling global efficient portfolios to be
# used with the zoo function rollapply()
#

rollGmin = function(x) {
	mu.hat = colMeans(x)
	cov.hat = var(x)
	gmin = globalMin.portfolio(er=mu.hat,cov.mat=cov.hat)
	ans = c(gmin$er,gmin$sd,gmin$weights)
	names(ans)[1:2] = c("er","sd")
	return(ans)
}
rollefficient = function(x,target=0.015) {
	mu.hat = colMeans(x)
	cov.hat = var(x)
	eport = efficient.portfolio(er=mu.hat,
                              cov.mat=cov.hat,
	                            target.return=target)
	ans = c(eport$er,eport$sd,eport$weights)
	names(ans)[1:2] = c("er","sd")
	return(ans)
}

# rolling 24-month global minimum variance portfolios
roll.gmin = rollapply(ret.z, width=24,
                      by.column=FALSE,align="right",
                      FUN=rollGmin)
colnames(roll.gmin)

# plot rolling weights in global min var portfolio
plot(roll.gmin[,3:6],main="",
     plot.type="single", col=1:4, lwd=3, ylab="weight")    
abline(h=0)
legend(x="bottomleft",legend=colnames(ret.z),
       lty=rep(1,4),col=1:4,lwd=3)

# show rolling weights in stacked barchart using PerformanceAnalytics function chart.StackedBar()
chart.StackedBar(roll.gmin[,3:6])
           
# plot rolling means and sds on global min variance portfolio
plot(roll.gmin[,1:2],plot.type="single",ylab="percent",
     main="",
     col=c("black","blue"),lwd=3)
abline(h=0)
legend(x="topleft",legend=c("Rolling mean","Rolling sd"),
       lty=rep(1,2),col=c("black","blue"),lwd=3)

# rolling efficient portfolios with target = 0.015
roll.eport = rollapply(ret.z, width=24,
                       by.column=F,align="right",
                       FUN=rollefficient)

# plot rolling weights
plot(roll.eport[,3:6],main="",
     plot.type="single", ylab="weight", col=1:4,lwd=3)
abline(h=0)
legend(x="bottomleft",legend=colnames(ret.z),
       lty=rep(1,4),col=1:4,lwd=3)
# stacked bar chart
chart.StackedBar(roll.eport[,3:6])


plot(roll.eport[,1:2], plot.type="single", ylab="percent",
     main="", ylim=c(0, 0.15),
     col=c("black","blue"),lwd=3)
abline(h=0)
legend(x="topleft",legend=c("Target er=0.015","Rolling sd"),
       lty=rep(1,2),col=c("black","blue"),lwd=3)
