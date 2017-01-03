# econ424lab7.r			script file for econ 424 lab7 calculations
#
# author: Eric Zivot
# created: October 20, 2003
# revised: 
# August 4, 2015 for Summer 2015
# May 18, 2015 for Spring 2015
#
# comments:

options(digits=3, width=70)

# make sure packages are installed prior to loading them
library(corrplot)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(zoo)


################################################################################
# introduction to portfolio theory
################################################################################

# load data from class website
lab7returns.df = read.csv(file="http://faculty.washington.edu/ezivot/econ424/424lab7returns.csv",
                          stringsAsFactors=FALSE)
# 7/31/12: fix to problem with the yearmon class
dates = seq(as.Date("1992-07-01"), as.Date("2000-10-01"), by="months")
lab7returns.df$Date = dates
# create zoo object
lab7returns.z = zoo(lab7returns.df[,-1], lab7returns.df$Date)
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab7returns.z, lwd=2, col="blue", panel = my.panel)

# compute estimates of CER model and annualize
muhat.annual = apply(lab7returns.z,2,mean)*12   
sigma2.annual = apply(lab7returns.z,2,var)*12
sigma.annual = sqrt(sigma2.annual)
covmat.annual = cov(lab7returns.z)*12 
covhat.annual = cov(lab7returns.z)[1,2]*12   
rhohat.annual = cor(lab7returns.z)[1,2]

mu.b = muhat.annual["rboeing"]
mu.m = muhat.annual["rmsft"]
sig2.b =  sigma2.annual["rboeing"]
sig2.m = sigma2.annual["rmsft"]
sig.b = sigma.annual["rboeing"]
sig.m = sigma.annual["rmsft"]
sig.bm = covhat.annual
rho.bm = rhohat.annual


#
# 3. create portfolios and plot
#
x.b = seq(from=-1, to=2, by=0.1)
x.m = 1 - x.b
mu.p = x.b*mu.b + x.m*mu.m
sig2.p = x.b^2 * sig2.b + x.m^2 * sig2.m + 2*x.b*x.m*sig.bm
sig.p = sqrt(sig2.p)

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)

# now compute portfolios with assets and T-bills as well as Sharpe slopes

r.f = 0.03
# T-bills + Boeing
x.b = seq(from=0, to=2, by=0.1)
mu.p.b = r.f + x.b*(mu.b - r.f)
sig.p.b = x.b*sig.b
sharpe.b = (mu.b - r.f)/sig.b
sharpe.b

# T-bills + MSFT
x.m = seq(from=0, to=2, by=0.1)
mu.p.m = r.f + x.m*(mu.m - r.f)
sig.p.m = x.m*sig.m
sharpe.m = (mu.m - r.f)/sig.m
sharpe.m

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
points(sig.p.b, mu.p.b, type="b", col="blue")
points(sig.p.m, mu.p.m, type="b", col="orange")


#
# 4. compute global minimum variance portfolio
#

gmin.port = globalMin.portfolio(muhat.annual,
                                covmat.annual) 
gmin.port
summary(gmin.port, risk.free=0.03)
plot(gmin.port)

pie(gmin.port$weights)

sharpe.gmin = (gmin.port$er - r.f)/gmin.port$sd
sharpe.gmin

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=gmin.port$sd, y=gmin.port$er, labels="Global min", pos=2)

#
# 5. compute tangency portfolio
#

tan.port = tangency.portfolio(muhat.annual,
                              covmat.annual,
                              risk.free=0.03) 
tan.port
summary(tan.port,risk.free=0.03)
plot(tan.port)
pie(tan.port$weights)                              

# T-bills + tangency
x.t = seq(from=0, to=2, by=0.1)
mu.p.t = r.f + x.t*(tan.port$er - r.f)
sig.p.t = x.t*tan.port$sd
sharpe.t = (tan.port$er - r.f)/tan.port$sd
sharpe.t


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)

#
# 6 
#

x.t = 0.1
x.f = 1 - x.t

mu.e = r.f + x.t*(tan.port$er - r.f)
sd.e = x.t*tan.port$sd
sharpe.e = (mu.e - r.f)/sd.e
sharpe.e


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)
points(sd.e, mu.e, type="p", col="orange", pch=16, cex=2)
text(x=sd.e, y=mu.e, labels="Efficient Portfolio with 10% Tangency", pos=4, cex=0.75)


################################################################################
# introduction to portfolio theory with matrix algebra
################################################################################

lab7.df = read.csv("http://faculty.washington.edu/ezivot/econ424/econ424lab7returns.csv",
                   stringsAsFactors=F)
colnames(lab7.df)

#
# Create zoo object from data and dates in lab7.df
#    

lab7.z = zoo(x=lab7.df[, -1], 
             order.by=as.yearmon(lab7.df[, 1], format="%b-%y"))
start(lab7.z)
end(lab7.z)
colnames(lab7.z)


#
# Create timePlots of data
#

# create custom panel function to draw horizontal line at zero in each panel
# of plot
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab7.z, lwd=2, panel=my.panel, col="blue")

# all on the same graph
plot(lab7.z, plot.type = "single", main="lab7 returns",
     col=1:4, lwd=2)
abline(h=0)
legend(x="bottomleft", legend=colnames(lab7.z), col=1:4, lwd=2)


#
# Compute pairwise scatterplots
#

pairs(coredata(lab7.z), col="blue", pch=16)
corrplot(cor(lab7.z), method="ellipse")
# clear the plots if use Rstudio

#
# Compute estimates of CER model parameters
#

muhat.vals = apply(lab7.z, 2, mean)
muhat.vals
sigma2hat.vals = apply(lab7.z, 2, var)
sigma2hat.vals
sigmahat.vals = apply(lab7.z, 2, sd)
sigmahat.vals
cov.mat = var(lab7.z)
cov.mat
cor.mat = cor(lab7.z)
cor.mat

#
#    Export means and covariance matrix to .csv file for
#    import to Excel. Be sure to change the directories to the appropriate ones on your
#    computer
#

write.csv(muhat.vals, file="C:\\Users\\ezivot\\Documents\\classes\\econ424\\fall2010\\muhatVals.csv")
write.csv(cov.mat, file="C:\\Users\\ezivot\\Documents\\classes\\econ424\\fall2010\\covMat.csv")

#
# portfolio theory calculations using functions in IntroCompFinR
#

# compute global minimum variance portfolio with short sales
gmin.port = globalMin.portfolio(muhat.vals, cov.mat)
gmin.port
plot(gmin.port, col="blue")

# compute efficient portfolio with target return equal to highest average return
mu.target = max(muhat.vals)
e1.port = efficient.portfolio(muhat.vals, cov.mat, mu.target)
e1.port
plot(e1.port, col="blue")


# compute covariance b/w min var portfolio and efficient port
t(gmin.port$weights)%*%cov.mat%*%e1.port$weights

# compute efficient frontier of risk assets and plot
e.frontier = efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, alpha.max=1)
summary(e.frontier)
plot(e.frontier, plot.assets=T, col="blue", pch=16, cex=2)

# compute tangency portfolio with rf = 0.005
tan.port = tangency.portfolio(muhat.vals, cov.mat, risk.free=0.005)
summary(tan.port)
plot(tan.port, col="blue")