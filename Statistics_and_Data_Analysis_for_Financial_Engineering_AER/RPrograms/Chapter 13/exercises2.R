library(faraway)
###########  Curvature present
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
y = x + .3*x^2 + rnorm(100,sd=3)
fit = lm(y~x)
yhat = fit$fit
resid = rstudent(fit)
postscript("exercises2_01.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)")
lines(lowess(x,resid),lwd=2)
plot(yhat,abs(resid),main="(b)")
lines(lowess(yhat,abs(resid)),lwd=2)
qqnorm(resid,datax=T,main="(c)",
   ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag")
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index")
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
graphics.off()



###########  leverage point
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
y = x + .3*x + rnorm(100,sd=3)
x[58] = x[58] + 16
fit = lm(y~x)
yhat = fit$fit
resid = rstudent(fit)
postscript("exercises2_02.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)")
lines(lowess(x,resid),lwd=2)
plot(yhat,abs(resid),main="(b)")
lines(lowess(yhat,abs(resid)),lwd=2)
qqnorm(resid,datax=T,main="(c)",
   ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag")
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index")
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
graphics.off()



###########  correlated noise
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
noise = as.vector(arima.sim(list(order = c(1,0,0), ar = 0.7), n = 100))
y = x + .3*x + noise

fit = lm(y~x)
yhat = fit$fit
resid = rstudent(fit)
postscript("exercises2_03.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)")
lines(lowess(x,resid),lwd=2)
plot(yhat,abs(resid),main="(b)")
lines(lowess(yhat,abs(resid)),lwd=2)
qqnorm(resid,datax=T,main="(c)",
   ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag")
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index")
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
graphics.off()



###########  skewness
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
noise = rnorm(100,sd=.6)
y = exp(.2*x + noise)

fit = lm(y~x)
yhat = fit$fit
resid = rstudent(fit)
postscript("exercises2_04.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)")
lines(lowess(x,resid),lwd=2)
plot(yhat,abs(resid),main="(b)")
lines(lowess(yhat,abs(resid)),lwd=2)
qqnorm(resid,datax=T,main="(c)",
   ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag")
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index")
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
graphics.off()