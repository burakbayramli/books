library(Ecdat)
library(KernSmooth)
library(locfit)
library(mgcv)

data(Capm)
attach(Capm)
n = length(rf)
year = seq(1960.125,2003,length=n)
diffrf=diff(Capm$rf)
rf_lag = rf[1:(n-1)]
log_rf_lag = log(rf_lag)

ll_mu <- locpoly(rf_lag,diffrf, bandwidth = dpill(rf_lag,diffrf) )
muhat = spline(ll_mu$x,ll_mu$y,xout=rf_lag)$y
epsilon_sqr = (diffrf-muhat)^2
ll_sig <- locpoly(rf_lag,epsilon_sqr, 
   bandwidth = dpill(rf_lag,epsilon_sqr) )

gam_mu = gam(diffrf~s(rf_lag,bs="cr"),method="REML")
epsilon_sqr = (diffrf-gam_mu$fit)^2
gam_sig = gam(epsilon_sqr~s(rf_lag,bs="cr"),method="REML")

locfit_mu = locfit(diffrf~rf_lag)
epsilon_sqr = (diffrf - fitted(locfit_mu))^2
locfit_sig = locfit(epsilon_sqr~rf_lag)

std_res = (diffrf - fitted(locfit_mu)) / sqrt(fitted(locfit_sig))


postscript("riskfree01.ps",width=6,height=5)  #  Figure 21.1
par(mfrow=c(2,2))
plot(year,rf,ylab="return",main="(a)",type="l" )
plot(year[2:n],diffrf,ylab="change in  return",main="(b)",type="l",xlab="year")
plot(rf_lag,diffrf,ylab="change in return",xlab="return",main="(c)",type="p",cex=.7)
lines(ll_mu$x,ll_mu$y,lwd=3)
plot(rf_lag,(diffrf-muhat)^2,xlab="return",ylab="squared residual",main="(d)",cex=.7)
lines(ll_sig$x,ll_sig$y,lwd=3)
graphics.off()

orrf = order(rf_lag)

postscript("riskfree02.ps",width=8,height=4)  #  Figure 21.6
par(mfrow=c(1,2))
plot(rf_lag[orrf],gam_mu$fit[orrf],type="l",lwd=3,lty=1,
   xlab="lagged rate",ylab="change in rate",main="(a)") 
lines(ll_mu$x,ll_mu$y,lwd=3,lty=2)
lines(locfit_mu,lwd=3,lty=3)
legend(.1,-.05,c("spline","local linear","local quadratic"),
   lty=c(1,2,3),cex=.85,lwd=3)
rug(rf_lag)
abline(h=0,lwd=2)
#graphics.off()

min(rf_lag[(gam_mu$fit < 0)])

#postscript("riskfree03.ps",width=6,height=5)
#par(mfrow=c(1,1))
plot(rf_lag[orrf],gam_sig$fit[orrf],type="l",lwd=3,lty=1,ylim=c(0,.03),  
   xlab="lagged rate",ylab="squared residual",main="(b)")
lines(ll_sig$x,ll_sig$y,lwd=3,lty=2)
lines(locfit_sig,lwd=3,lty=3)
abline(h=0,lwd=2)
legend("topleft",c("spline","local linear","local quadratic"),
   lty=c(1,2,3),cex=.85,lwd=3)
rug(rf_lag)
graphics.off()


postscript("riskfree04.ps",width=7,height=3)  #  Figure 21.7
par(mfrow=c(1,3))
plot(year[2:n],std_res,type="l",xlab="year",ylab="",main="(a) standardized residuals")
acf(std_res,main="(b) standardized residuals")
acf(std_res^2,main="(c) squared standardized residuals")
graphics.off()


##################################


plot(gcvplot(epsilon_sqr~rf_lag,alpha=seq(0.2,1,by=0.01)))
plot(lcvplot(epsilon_sqr~rf_lag,deg=1,alpha=seq(0.1,.5,by=0.01)))
lo_mu = loess(diffrf~rf_lag)
plot(rf_lag,predict(lo_mu))
abline(h=0)
lo_sig = loess(epsilon_sqr~rf_lag)
plot(rf_lag,predict(lo_sig))









