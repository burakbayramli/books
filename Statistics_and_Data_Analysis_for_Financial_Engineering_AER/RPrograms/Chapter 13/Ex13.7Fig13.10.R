#  Example 13.7 and Figure 13.10

dat = read.table(file="WeekInt.txt",header=T)
library("car")
library("lmtest")
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )


fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
durbinWatsonTest(fit2,max.lag=1,rep=10000)
dwtest(fit2,alternative="two.sided",exact=T)

resid = residuals(fit2)
auto.arima(resid,ic="aic")

n = length(resid)
tt = (1:n)/(n+1)

fit_t = fitdistr(resid,"t")
library(forecast)
auto.arima(resid,ic="aic")
auto.arima(resid,ic="bic")

postscript("WeekInt_2var_residuals.ps",height=6,width=6)   #  Figure 13.0
par(mfrow=c(2,2),lwd=1)
qqnorm(resid,datax=T,main="(a) Normal plot",xlab="theoretical quantiles",
   ylab="sample quantiles")
qqline(resid,datax=T)
qqplot(resid, (-0.00035 +0.04058 * qt(tt,df=3)) ,main="(b) t-plot",
  ylab="theoretical Quantiles")
abline(0,1)
acf(resid,main="(c)",xlab="lag")
plot(fitted(fit2),resid,main="(d)",ylab="Residuals",xlab="fitted values")
graphics.off()
