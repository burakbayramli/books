###########################  additive model, wages  ####
library(AER)
library(mgcv)
data(CPS1988)
attach(CPS1988)
fitGam = gam(log(wage)~s(education)+s(experience)+ethnicity)
summary(fitGam)
postscript("CPSGamFit.ps",width=6,height=3.5)
par(mfrow=c(1,2))
plot(fitGam)
graphics.off()



100* mean((education < 10))

100* mean((experience > 50))


#####################  CKLS, extended
library(Ecdat)
data(Irates)
r1 = Irates[,1]
n = length(r1)
lag_r1 = lag(r1)[-n]
delta_r1 = diff(r1)
n = length(lag_r1)
knots = seq(from=1950,to=1985,length=10)
t = seq(from=1946,to =1991+2/12,length=n)
X1 = outer(t,knots,FUN="-")
X2 = X1 * (X1>0)
X3 = cbind(rep(1,n), (t - 1946),X2)
m2 = dim(X3)[2]
m = m2 - 1

nlmod_CKLS_ext = nls(delta_r1 ~ X3[,1:2]%*%a * (X3%*%theta-lag_r1),
   start=list(theta = c(10,rep(0,m)),
   a=c(.01,0)),control=list(maxiter=200))
AIC(nlmod_CKLS_ext)
param4 = summary(nlmod_CKLS_ext)$parameters[,1]
par(mfrow=c(1,3))
plot(t,X3%*%param4[1:m2],ylim=c(0,16),ylab="rate",
   main="(a)",col="red",type="l",lwd=2)
lines(t,lag_r1)
legend("topleft",c("theta(t)","lagged rate"),lwd=c(2,1),col=c("red","black"))
      
plot(t,X3[,1:2]%*%param4[(m2+1):(m2+2)],ylab="a(t)",
   col="red",type="l",lwd=2,main="(b)")

res_sq = residuals(nlmod_CKLS_ext)^2
nlmod_CKLS_ext_res <- nls(res_sq ~  A*lag_r1^B,
   start=list(A=.2,B=1/2) )

plot(lag_r1,sqrt(res_sq),pch=5,ylim=c(0,6),ylab="",main="(c)")
lines(lag_r1,sqrt(fitted(nlmod_CKLS_ext_res)),
   lw=3,col="red",type="l")
legend("topleft",c("abs res","volatility fn"),lty=c(NA,1),
   pch=c(5,NA),col=c("black","red"),lwd=1:2)

###########  nonparametric factor model
library(mgcv)
dat = read.csv("capm.csv",header=T)
attach(dat)
n = dim(dat)[1]
EX_R_sp500 = Close.sp500[2:n]/Close.sp500[1:(n-1)] - 1  - Close.tbill[2:n]/(100*253) 
EX_R_msft = Close.msft[2:n]/Close.msft[1:(n-1)] - 1  - Close.tbill[2:n]/(100*253) 
EX_R_Ford = Close.ford[2:n]/Close.ford[1:(n-1)] - 1  - Close.tbill[2:n]/(100*253) 
fitGam = gam(EX_R_Ford~s(EX_R_sp500,bs="cr"),method="REML")
AIC(fitGam)
plot(fitGam)









