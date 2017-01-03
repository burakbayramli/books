# introductionToPortfolioTheory.r
#
# Examples used in Introduction to Portfolio Theory Chapter
#
# Author: Eric Zivot
# Created: November 8, 2009
# Revision history:
# July 30, 2013
#   Update for Summer 2013 class
# July 26, 2012
#   Updated for Summer 2012 class
# August 4, 2011
#   Updated for Summer 2011 class
# November 11, 2010
#   Updated for Fall 2010 class
# November 15, 2009
#   Updated code to go along with examples in chapter
#

options(digits=4, width=70)
library(PerformanceAnalytics)

################################################################################
# 2 asset example
################################################################################
mu.A = 0.175
sig.A = 0.258
sig2.A = sig.A^2
mu.B = 0.055
sig.B = 0.115
sig2.B = sig.B^2
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B
w0 = 100000
VaR.A = (mu.A + sig.A*qnorm(0.05))*w0
VaR.A
VaR.B = (mu.B + sig.B*qnorm(0.05))*w0
VaR.B

#
# example portfolios and VaR
#
x.A = 0.5
x.B = 0.5
mu.p1 = x.A*mu.A + x.B*mu.B
sig2.p1 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p1 = sqrt(sig2.p1)
mu.p1
sig2.p1
sig.p1
VaR.p1 = (mu.p1 + sig.p1*qnorm(0.05))*w0
VaR.p1
# note: portfolio VaR is not a weighted average of individual asset VaR
x.A*VaR.A + x.B*VaR.B


x.A = 1.5
x.B = -0.5
mu.p2 = x.A*mu.A + x.B*mu.B
sig2.p2 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p2 = sqrt(sig2.p2)
mu.p2
sig2.p2
sig.p2
VaR.p2 = (mu.p2 + sig.p2*qnorm(0.05))*w0
VaR.p2
# note: portfolio VaR is not a weighted average of individual asset VaR
x.A*VaR.A + x.B*VaR.B

# function to compute normal VaR
normalVaR <- function(mu, sigma, w0, tail.prob = 0.01, invert=FALSE) {
## compute normal VaR for collection of assets given mean and sd vector
## inputs:
## mu         n x 1 vector of expected returns
## sigma      n x 1 vector of standard deviations
## w0         scalar initial investment in $
## tail.prob  scalar tail probability
## invert     logical. If TRUE report VaR as positive number
## output:
## VaR        n x 1 vector of left tail return quantiles
## References:
## Jorian (2007) pg 111.
  if ( length(mu) != length(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  VaR = w0*(mu + sigma*qnorm(tail.prob))
  if (invert) {
    VaR = -VaR
  }
  return(VaR)
}

# compute VaR on three assets

normalVaR(mu=c(mu.A, mu.B, mu.p1),
          sigma=c(sig.A, sig.B, sig.p1),
          w0=100000, tail.prob=0.05) 
  
################################################################################
# efficient portfolios
################################################################################

x.A = seq(from=-0.4, to=1.4, by=0.1)
x.B = 1 - x.A
mu.p = x.A*mu.A + x.B*mu.B
sig2.p = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p = sqrt(sig2.p)

# minimum variance portfolio
xA.min = (sig2.B - sig.AB)/(sig2.A + sig2.B - 2*sig.AB)
xB.min = 1 - xA.min
xA.min
xB.min
mu.p.min = xA.min*mu.A + xB.min*mu.B
sig2.p.min = xA.min^2 * sig2.A + xB.min^2 * sig2.B + 2*xA.min*xB.min*sig.AB
sig.p.min = sqrt(sig2.p.min)
mu.p.min
sig.p.min

# create portfolio plot
cex.val = 2
plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]), cex.lab=cex.val,
     col=c(rep("red", 6), "blue", rep("green", 12)))
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
text(x=sig.p.min, y=mu.p.min, labels="Global min", pos=2, cex = cex.val)

#
# portfolio frontier with varying values of rho
#
rho.AB.vals = c(-0.9,-0.5, -0.25, 0, 0.25, 0.5, 0.9)
x.A = seq(from=-0.4, to=1.4, by=0.1)
x.B = 1 - x.A
mu.p = x.A*mu.A + x.B*mu.B
sig2.p = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*rho.AB.vals[1]*sig.A*sig.B
sig.p = sqrt(sig2.p)

# create portfolio plot
cex.val = 1.5
plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     cex.lab = cex.val, col=1)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)

for (i in 2:length(rho.AB.vals)) {
  sig2.p = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*rho.AB.vals[i]*sig.A*sig.B
  sig.p = sqrt(sig2.p)
  points(sig.p, mu.p, type="b", pch=16, col=i, cex=cex.val)
}
legend(x="bottomright", legend=paste("rho", as.character(rho.AB.vals), sep="="), 
       col=1:length(rho.AB.vals), lty=1, pch=16, cex = cex.val)

#
# show portfolio plot with rho = 1 and rho = -1
#
rho.AB = 1
sig.AB = rho.AB*sig.A*sig.B
mu.p.1 = x.A*mu.A + x.B*mu.B
sig2.p.1 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p.1 = sqrt(sig2.p.1)
rho.AB = -1
sig.AB = rho.AB*sig.A*sig.B
mu.p.m1 = x.A*mu.A + x.B*mu.B
sig2.p.m1 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p.m1 = sqrt(sig2.p.m1)

plot(sig.p.1, mu.p.1, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
points(sig.p.m1, mu.p.m1, type="b", col="blue", pch=22, cex = cex.val)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
legend(x="topleft", legend=c(expression(rho==1), expression(rho==-1)),
       col=c("black", "blue"), pch=c(16,22), cex = cex.val)

#
# show efficient portfolios
#
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B
x.A = seq(from=-0.4, to=1.4, by=0.1)
x.B = 1 - x.A
mu.p = x.A*mu.A + x.B*mu.B
sig2.p = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p = sqrt(sig2.p)
plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), cex = cex.val,
     xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("red", 6), rep("green", 13)))
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
segments(x0=sig.p[3], y0=0, x1=sig.p[3], y1=mu.p[11], 
         lwd=3, lty="dotted")
text(x=sig.p[3], y=mu.p[3], labels="Inefficient portfolio", 
     pos=4, cex = cex.val)
text(x=sig.p[3], y=mu.p[11], labels="Efficient portfolio", 
     cex = cex.val, pos=4)


#
# portfolios with 1 risky asset and T-Bills
#

r.f = 0.03
# T-bills + asset A
x.A = seq(from=0, to=1.4, by=0.1)
mu.p.A = r.f + x.A*(mu.A - r.f)
sig.p.A = x.A*sig.A
sharpe.A = (mu.A - r.f)/sig.A
sharpe.A
# T-bills + asset B
x.B = seq(from=0, to=1.4, by=0.1)
mu.p.B = r.f + x.B*(mu.B - r.f)
sig.p.B = x.B*sig.B
sharpe.B = (mu.B - r.f)/sig.B
sharpe.B


# plot portfolios of T-Bills and assets A and B
plot(sig.p.A, mu.p.A, type="b", col="green", ylim=c(0, max(mu.p.A)),
     xlim=c(0, max(sig.p.A, sig.p.B)), pch=16, cex = cex.val,
     xlab=expression(sigma[p]), ylab=expression(mu[p]), cex.lab = cex.val)
points(sig.p.B, mu.p.B, type="b", col="red", pch=16, cex = cex.val)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=1, cex = cex.val)
text(x=0, y=r.f, labels=expression(r[f]), pos=2, cex = cex.val)

#
# portfolios of 2 risky assets and T-Bills
#
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B
top = (mu.A - r.f)*sig2.B - (mu.B - r.f)*sig.AB
bot = (mu.A - r.f)*sig2.B + (mu.B - r.f)*sig2.A - (mu.A - r.f + mu.B - r.f)*sig.AB
x.A.tan = top/bot
x.B.tan = 1 - x.A.tan
x.A.tan
x.B.tan
mu.p.tan = x.A.tan*mu.A + x.B.tan*mu.B
sig2.p.tan = x.A.tan^2 * sig2.A + x.B.tan^2 * sig2.B + 2*x.A.tan*x.B.tan*sig.AB
sig.p.tan = sqrt(sig2.p.tan)
mu.p.tan
sig.p.tan

# T-bills plus tangency
x.tan = seq(from=0, to=2.4, by=0.1)
mu.p.tan.tbill = r.f + x.tan*(mu.p.tan - r.f)
sig.p.tan.tbill = x.tan*sig.p.tan

# plot portfolios w/o tangency portfolio
plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]), cex.lab = cex.val)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
text(x=sig.p.min, y=mu.p.min, labels="Global min", pos=4, cex = cex.val)
text(x=0, y=r.f, labels=expression(r[f]), pos=2, cex = cex.val)
text(x=sig.p.tan, y=mu.p.tan, labels="Tangency", pos=2, cex = cex.val)
points(sig.p.A, mu.p.A, type="b", col="red", pch=16, cex = cex.val)
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16, cex = cex.val)

# plot portfolios with tangency portfolio
plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]), cex.lab = cex.val)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
text(x=sig.p.min, y=mu.p.min, labels="Global min", pos=4, cex = cex.val)
text(x=0, y=r.f, labels=expression(r[f]), pos=2, cex = cex.val)
text(x=sig.p.tan, y=mu.p.tan, labels="Tangency", pos=2, cex = cex.val)
points(sig.p.A, mu.p.A, type="b", col="red", pch=16, cex = cex.val)
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16, cex = cex.val)
points(sig.p.tan.tbill, mu.p.tan.tbill, type="b", col="green", pch=16, cex = cex.val)

#
# interpreting efficient portfolios
#

# safe portfolio: 10% in tangency and 90% in T-bills
mu.safe = r.f + 0.10*(mu.p.tan - r.f)
sig.safe = 0.10*sig.p.tan
# risky portfolio: 110% in tangency and -10% in T-bills
mu.risky = r.f + 2*(mu.p.tan - r.f)
sig.risky = 2*sig.p.tan
plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)), cex.lab = cex.val,
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
text(x=sig.p.min, y=mu.p.min, labels="Global min", pos=4, cex = cex.val)
text(x=0, y=r.f, labels=expression(r[f]), pos=2, cex = cex.val)
text(x=sig.p.tan, y=mu.p.tan, labels="Tangency", pos=2, cex = cex.val)
points(x=sig.p.tan, y=mu.p.tan, pch=16, cex=2, col="green")
points(sig.p.tan.tbill, mu.p.tan.tbill, type="b", col="green", 
       pch=16, cex = cex.val)
points(x=sig.safe, y=mu.safe, pch=16, cex=2)
text(x=sig.safe, y=mu.safe, labels="Safe", pos=3, cex = cex.val)
points(x=sig.risky, y=mu.risky, pch=16, cex=2)
text(x=sig.risky, y=mu.risky, labels="Risky", pos=3, cex = cex.val)

# find combination of T-bills and tangency that has same SD as asset B
x.t1 = sig.B/sig.p.tan
x.t1
mu.p1.e = r.f + x.t1*(mu.p.tan - r.f)
mu.p1.e
sig.p1.e = x.t1*sig.p.tan
sig.p1.e

# find combination of T-bills and tangency that has same ER as asset B
x.t2 = (mu.B - r.f)/(mu.p.tan - r.f)
x.t2
mu.p2.e = r.f + x.t2*(mu.p.tan - r.f)
mu.p2.e
sig.p2.e = x.t2*sig.p.tan
sig.p2.e

plot(sig.p, mu.p, type="b", pch=16, cex = cex.val,
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
text(x=sig.A, y=mu.A, labels="Asset A", pos=4, cex = cex.val)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4, cex = cex.val)
text(x=sig.p.min, y=mu.p.min, labels="Global min", pos=4, cex = cex.val)
text(x=0, y=r.f, labels=expression(r[f]), pos=2, cex = cex.val)
text(x=sig.p.tan, y=mu.p.tan, labels="Tangency", pos=4, cex = cex.val)
points(x=sig.p.tan, y=mu.p.tan, pch=16, cex=1.5, col="green", cex = cex.val)
points(sig.p.tan.tbill, mu.p.tan.tbill, type="b", col="green", pch=16, cex = cex.val)
segments(x0=sig.B, y0=0, x1=sig.B, y1=mu.p1.e, lwd=3, lty="dotted")
segments(x0=sig.B, y0=mu.p1.e, x1=0, y1=mu.p1.e, lwd=3, lty="dotted")
segments(x0=sig.p2.e, y0=0, x1=sig.p2.e, y1=mu.p2.e, lwd=3, lty="dotted")
segments(x0=0, y0=mu.p2.e, x1=sig.B, y1=mu.p2.e, lwd=3, lty="dotted")
text(x=sig.B, y=mu.p1.e, labels="e1", pos=3, cex = cex.val)
text(x=sig.p2.e, y=mu.B, labels="e2", pos=3, cex = cex.val)


################################################################################
# risk budgeting
################################################################################

riskBudgetSD <- function(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB) {
  sig.p = sqrt(x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB)
  mcr.A = (x.A*sig2.A + x.B*sig.AB)/sig.p
  mcr.B = (x.B*sig2.B + x.A*sig.AB)/sig.p
  cr.A = x.A*mcr.A
  cr.B = x.B*mcr.B
  pcr.A = cr.A/sig.p
  pcr.B = cr.B/sig.p
  ans = list(sig.p=sig.p,
             x=c(x.A, x.B),
             mcr = c(mcr.A, mcr.B),
             cr = c(cr.A, cr.B),
             pcr = c(pcr.A, pcr.B))
  return(ans)
}             
x.A = 0.5
x.B = 0.5
rb.sd = riskBudgetSD(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB)             
rb.sd
x.A = 1.5
x.B = -0.5
rb.sd = riskBudgetSD(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB)             
rb.sd

riskBudgetVaR <- function(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB, 
                          w0=100000, alpha=0.05) {
  mu.p = x.A*mu.A + x.B*mu.B
  sig.p = sqrt(x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB)
  VaR.p = (mu.p + sig.p*qnorm(0.05))*w0
  mcr.sig.A = (x.A*sig2.A + x.B*sig.AB)/sig.p
  mcr.sig.B = (x.B*sig2.B + x.A*sig.AB)/sig.p
  mcr.A = (mu.A + mcr.sig.A*qnorm(alpha))*w0
  mcr.B = (mu.B + mcr.sig.B*qnorm(alpha))*w0
  cr.A = x.A*mcr.A
  cr.B = x.B*mcr.B
  pcr.A = cr.A/VaR.p
  pcr.B = cr.B/VaR.p
  ans = list(VaR.p=VaR.p,
             x=c(x.A, x.B),
             mcr = c(mcr.A, mcr.B),
             cr = c(cr.A, cr.B),
             pcr = c(pcr.A, pcr.B))
  return(ans)
}   
x.A = 0.5
x.B = 0.5
rb.VaR = riskBudgetVaR(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB)             
rb.VaR
x.A = 1.5
x.B = -0.5
rb.VaR = riskBudgetVaR(x.A, x.B, mu.A, mu.B, sig2.A, sig2.B, sig.AB)             
rb.VaR

        