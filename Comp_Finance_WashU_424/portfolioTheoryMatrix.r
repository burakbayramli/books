# PortfolioTheoryMatrix.r
#
# Examples used in Portfolio Theory with Matrix Algebra Chapter
#
# Author: Eric Zivot
# Created: November 15, 2009
# Revision history:
# August 1, 2013
#   updated for summer 2013
# July 31, 2012
#   updated for summer 2012
# November 9, 2011
#   updated for summer 2011. Updated to work with R 2.12.2

options(digits=4, width=70)
library(PerformanceAnalytics)
# load portfolio theory functions
# source portfolio functions
source(file="http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r")

cex.val = 2

################################################################################
# 3 asset example
################################################################################
asset.names <- c("MSFT", "NORD", "SBUX")
mu.vec = c(0.0427, 0.0015, 0.0285)
names(mu.vec) = asset.names
sigma.mat = matrix(c(0.0100, 0.0018, 0.0011,
		   0.0018, 0.0109, 0.0026,
		   0.0011, 0.0026, 0.0199),
		 nrow=3, ncol=3)
dimnames(sigma.mat) = list(asset.names, asset.names)
mu.vec
sigma.mat

#
# show assets in mean-sd space
#
sd.vec = sqrt(diag(sigma.mat))
plot(sd.vec, mu.vec,  ylim=c(0, 0.06), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)

#
# equally weighted portfolio
#
x.vec = rep(1,3)/3
names(x.vec) = asset.names
sum(x.vec)
mu.p.x = crossprod(x.vec,mu.vec)
sig2.p.x = t(x.vec)%*%sigma.mat%*%x.vec
sig.p.x = sqrt(sig2.p.x)
mu.p.x
sig.p.x

# long-short portfolio
y.vec = c(0.8, 0.4, -0.2)
names(y.vec) = asset.names
sum(y.vec)
mu.p.y = crossprod(y.vec,mu.vec)
sig2.p.y = t(y.vec)%*%sigma.mat%*%y.vec
sig.p.y = sqrt(sig2.p.y)
mu.p.y
sig.p.y

# covariance and correlation between equally weighted and long-short portfolios
sig.xy = t(x.vec)%*%sigma.mat%*%y.vec
sig.xy
rho.xy = sig.xy/(sig.p.x*sig.p.y)
rho.xy

#
# show assets and portfolios in mean-sd space
#
cex.val = 1.5
plot(sd.vec, mu.vec,  ylim=c(0, 0.06), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
points(sig.p.x, mu.p.x, pch=16, col="black", cex=2.5)
text(sig.p.x, mu.p.x, labels="EQUAL WEIGHT", pos=4, cex = cex.val)
points(sig.p.y, mu.p.y, pch=16, col="black", cex=2.5)
text(sig.p.y, mu.p.y, labels="LONG-SHORT", pos=4, cex = cex.val)

#
# Compute and plot random portfolios to fill space
#
set.seed(123)
x.msft = runif(100, min=-1.5, max=1.5)
x.nord = runif(100, min=-1.5, max=1.5)
x.sbux = 1 - x.msft - x.nord

plot(sd.vec, mu.vec,  ylim=c(-0.03, 0.08), xlim=c(0, 0.4), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
for (i in 1:length(x.msft)) {
  z.vec = c(x.msft[i], x.nord[i], x.sbux[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=1.5)
}

#
# compute global minimum variance portfolio
#

# method 1: use full system matrix algebra
top.mat = cbind(2*sigma.mat, rep(1, 3))
bot.vec = c(rep(1, 3), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, 3), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:3,1]
m.vec
# compute expected return, variance and sd
mu.gmin = as.numeric(crossprod(m.vec, mu.vec))
mu.gmin
sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)
sig.gmin = sqrt(sig2.gmin)
sig2.gmin  
sig.gmin

# method 2: direct calculation of m using matrix algebra
one.vec = rep(1, 3)
sigma.inv.mat = solve(sigma.mat)
top.mat = sigma.inv.mat%*%one.vec
bot.val = as.numeric((t(one.vec)%*%sigma.inv.mat%*%one.vec))
m.mat = top.mat/bot.val
m.mat[,1]

# check with portfolio function
gmin.port <- globalMin.portfolio(mu.vec, sigma.mat)
gmin.port

# plot global minimum variance portfolio
plot(sd.vec, mu.vec,  ylim=c(-0.03, 0.08), xlim=c(0, 0.4), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
points(sig.gmin, mu.gmin, pch=16, cex=2.5, col="green")
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2.5, cex = cex.val)
for (i in 1:length(x.msft)) {
  z.vec = c(x.msft[i], x.nord[i], x.sbux[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=1.5)
}


# 
# find efficient portfolio with same ER as MSFT
#
top.mat = cbind(2*sigma.mat, mu.vec, rep(1, 3))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, 3), 0, 0)
Ax.mat = rbind(top.mat, mid.vec, bot.vec)
bmsft.vec = c(rep(0, 3), mu.vec["MSFT"], 1)
z.mat = solve(Ax.mat)%*%bmsft.vec
x.vec = z.mat[1:3,]
x.vec
# compute mean, variance and std deviation
mu.px = as.numeric(crossprod(x.vec, mu.vec))
mu.px
sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)
sig.px = sqrt(sig2.px)
sig.px
mu.vec["MSFT"]
sd.vec["MSFT"]

# 
# find efficient portfolio with same ER as SBUX
#

bsbux.vec = c(rep(0, 3), mu.vec["SBUX"], 1)
z.mat = solve(Ax.mat)%*%bsbux.vec
y.vec = z.mat[1:3,]
y.vec
# compute mean, variance and std deviation
mu.py = as.numeric(crossprod(y.vec, mu.vec))
sig2.py = as.numeric(t(y.vec)%*%sigma.mat%*%y.vec)
sig.py = sqrt(sig2.py)
mu.py
sig.py
mu.vec["SBUX"]
sd.vec["SBUX"]


# covariance and correlation between two portfolio returns
sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
rho.xy = sigma.xy/(sig.px*sig.py)
sigma.xy
rho.xy

#
# show assets and portfolios in mean-sd space
#
sd.vec = sqrt(diag(sigma.mat))
plot(sd.vec, mu.vec,  ylim=c(0, 0.06), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2)
points(sig.gmin, mu.gmin, pch=16, cex=2, col="green")
points(sig.px, mu.px, pch=16, cex=2, col="green")
points(sig.py, mu.py, pch=16, cex=2, col="green")      
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, cex = cex.val)        
text(sig.px, mu.px, labels="E1", pos=2, cex = cex.val) 
text(sig.py, mu.py, labels="E2", pos=2, cex = cex.val) 


# 
# find efficient portfolio with same ER as MSFT using alternative derivation
#

M.mat = cbind(mu.vec, one.vec)
B.mat = t(M.mat)%*%solve(sigma.mat)%*%M.mat
mu.tilde.msft = c(mu.vec["MSFT"], 1)
x.vec.2 = solve(sigma.mat)%*%M.mat%*%solve(B.mat)%*%mu.tilde.msft
# compare with previous solution
x.vec.2
x.vec

#
# find efficient portfolio from two efficient portfolios
#
a = 0.5
z.vec = a*x.vec + (1-a)*y.vec
z.vec
# compute mean, variance and std deviation
sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
mu.pz = as.numeric(crossprod(z.vec, mu.vec))
sig2.pz = as.numeric(t(z.vec)%*%sigma.mat%*%z.vec)
sig.pz = sqrt(sig2.pz)
mu.pz
sig.pz

# alternative calculation
mu.pz = a*mu.px + (1-a)*mu.py
sig.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
sig2.pz = a^2 * sig2.px + (1-a)^2 * sig2.py + 2*a*(1-a)*sig.xy
sig.pz = sqrt(sig2.pz)          
mu.pz
sig2.pz
sig.pz          



#
# find efficient portfolio with same er as nordstrom
#

a.nord = (mu.vec["NORD"] - mu.py)/(mu.px - mu.py)
a.nord
z.nord = a.nord*x.vec + (1 - a.nord)*y.vec
z.nord

mu.pz.nord = a.nord*mu.px + (1-a.nord)*mu.py
sig2.pz.nord = a.nord^2 * sig2.px + (1-a.nord)^2 * sig2.py + 2*a.nord*(1-a.nord)*sigma.xy          
sig.pz.nord = sqrt(sig2.pz.nord)          
mu.pz.nord
sig2.pz.nord
sig.pz.nord     

#
# find efficient portfolio with er = 0.05
#

a.05 = (0.05 - mu.py)/(mu.px - mu.py)
a.05
z.05 = a.05*x.vec + (1 - a.05)*y.vec
z.05
# compute mean, var and sd
mu.pz.05 = a.05*mu.px + (1-a.05)*mu.py
sig2.pz.05 = a.05^2 * sig2.px + (1-a.05)^2 * sig2.py + 2*a.05*(1-a.05)*sigma.xy          
sig.pz.05 = sqrt(sig2.pz.05)          
mu.pz.05
sig.pz.05

# show 4 efficient frontier portfolios 
sd.vals = c(sig.px, sig.py, sig.pz)
mu.vals = c(mu.px, mu.py, mu.pz)
plot(sd.vals, mu.vals,  ylim=c(0, 0.06), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="green", cex=2) 
points(sig.gmin, mu.gmin, pch=16, cex=2, col="green")     
points(sd.vec, mu.vec, pch=16, col="blue", cex=2)
points(sig.pz.05, mu.pz.05, pch=16, col="green", cex=2) 
text(sd.vals, mu.vals, labels=c("E1","E2","E3"), pos=2, cex = cex.val)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, cex = cex.val)        
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
text(sig.pz.05, mu.pz.05, labels="E4", pos=2, cex = cex.val)    


target.return <- mu.vec[1]
e.port.msft <- efficient.portfolio(mu.vec, sigma.mat, target.return)
e.port.msft

#
# compute and plot efficient frontier
#
a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, 3)
colnames(z.mat) = names(mu.vec)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(0, 0.06), xlim=c(0, 0.16), 
     pch=16, col="green", cex = cex.val, ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sd.vec, mu.vec, pch=16, cex=2, lwd=2, col="blue")
points(sig.gmin, mu.gmin, pch=16, col="green", cex=2)
points(sig.px, mu.px, pch=16, col="green", cex=2)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, cex = cex.val)
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
text(sig.px, mu.px, labels="E1", pos=2, cex = cex.val)

#
# plot weights as stacked barchart
#

# for some reason xlab doesn't show up
chart.StackedBar(z.mat, xaxis.labels=round(sqrt(sig2.z),digits=3), 
                 xlab="Portfolio SD", ylab="Weights")

#
# compute and plot efficient frontier and random portfolios
#
a = seq(from=1, to=-2, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, 3)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(-0.03, 0.08), xlim=c(0, 0.4), 
     pch=16, col="green", cex = cex.val, ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sd.vec, mu.vec, pch=16, cex=2, col="blue")

for (i in 1:length(x.msft)) {
  z.vec = c(x.msft[i], x.nord[i], x.sbux[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=1.5)
}

text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, col="green", cex = cex.val)
text(sd.vec, mu.vec, labels=asset.names, pos=4, col="blue", cex = cex.val)

#
# compute tangency portfolio
#

rf = 0.005
sigma.inv.mat = solve(sigma.mat)
one.vec = rep(1, 3)
mu.minus.rf = mu.vec - rf*one.vec
top.mat = sigma.inv.mat%*%mu.minus.rf
bot.val = as.numeric(t(one.vec)%*%top.mat)
t.vec = top.mat[,1]/bot.val
t.vec

# compute mean, var and sd
mu.t = as.numeric(crossprod(t.vec, mu.vec))
sig2.t = as.numeric(t(t.vec)%*%sigma.mat%*%t.vec)
sig.t = sqrt(sig2.t)
mu.t
sig.t

# sharpe ratio on tangency portfolio
sr.t = (mu.t - rf)/sig.t
sr.t


#
# Efficient portfolios of T-bills and tangency portfolio
#
x.t = seq(0, 2, by=0.1)
mu.pe = rf + x.t*(mu.t - rf)
sig.pe = x.t*sig.t 
slope.t = (mu.t - rf)/sig.t

# plot efficient portfolios
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(0, 0.08), xlim=c(0, 0.17), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))
abline(a=rf, b=slope.t, col="green", lwd=2)
points(sig.t, mu.t, pch=16, col="green", cex=2)
points(sd.vec, mu.vec, pch=16, cex=2, col="black")
text(sig.gmin, mu.gmin, labels="Global min", pos=4, cex=2)
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex=2)
text(sig.t, mu.t, labels="tangency", pos=4,cex=2)
text(0, rf, labels="Rf", pos=2, cex=2)

# plot weights
t.mat = x.t %o% t.vec
e.mat = cbind(1-x.t, t.mat)
colnames(e.mat)[1] = "T-Bill"
chart.StackedBar(e.mat, xaxis.labels=round(sig.pe,digits=3), 
                 xlab="Portfolio SD", ylab="Weights")


# find efficient portfolio with target volatility (risk) equal 0.02
x.t.02 = 0.02/sig.t
x.t.02
1-x.t.02
# shares in msft, nord and sbux
x.t.02*t.vec

# comute mean, var and sd
mu.t.02 = x.t.02*mu.t + (1-x.t.02)*rf
sig.t.02 = x.t.02*sig.t
mu.t.02
sig.t.02

# find efficient portfolio with target expected return equal to 0.07
x.t.07 = (0.07 - rf)/(mu.t - rf)
x.t.07
1-x.t.07
# shares in msft, nord and sbux
x.t.07*t.vec

# compute mean, var and sd
mu.t.07 = x.t.07*mu.t + (1-x.t.07)*rf
sig.t.07 = x.t.07*sig.t
mu.t.07
sig.t.07


# plot efficient portfolios
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(0, 0.08), xlim=c(0, 0.17), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))
abline(a=rf, b=slope.t, col="green", lwd=2)
points(sig.t, mu.t, pch=16, col="green", cex=2)
points(sig.t.02, mu.t.02, pch=16, col="green", cex=2)
points(sig.t.07, mu.t.07, pch=16, col="green", cex=2)
points(sd.vec, mu.vec, pch=16, col="black", cex=2)
text(sig.gmin, mu.gmin, labels="Global min", pos=4, cex=cex.val)
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex=cex.val)
text(sig.t, mu.t, labels="tangency", pos=4, cex=cex.val)
text(sig.t.02, mu.t.02, labels="E1", pos=3, cex=cex.val)
text(sig.t.07, mu.t.07, labels="E2", pos=3, cex=cex.val)




