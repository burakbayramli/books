library("faraway")
library("car")
library("Ecdat")

e = arima.sim(n=1250,list(ar=.7,ma=.2),sd = .2)
x = rnorm(1250)
y = x + e
fit01=arima(y,order=c(1,0,1),xreg=x)






data(Icecream)
fit_ic_lm = lm(cons~income+price+temp,data=Icecream)
summary(fit_ic_lm)
Box.test(fit_ic_lm$resid)

durbin.watson(fit_ic_lm)
acf(fit_ic_lm$resid)
attach(Icecream)
fit_ic_ma = arima(cons,order=c(0,0,1),xreg=cbind(income,price,temp))
fit_ic_ar = arima(cons,order=c(1,0,0),xreg=cbind(income,price,temp))
options(digits=3)
print(fit_ic_ma,digits=5)
print(fit_ic_ar,digits=5)

postscript("ice_cream_acf_resid.ps",width=6.5,height=2.75)
par(mfrow=c(1,3))
acf(fit_ic_lm$resid,main="linear model/indep. noise",xlab="lag")
acf(fit_ic_ar$resid,main="linear model/AR(1) noise",xlab="lag")
acf(fit_ic_ma$resid,main="linear model/MA(1) noise",xlab="lag")
graphics.off()

summary(lm(cons~temp))
summary(lm(cons~income+temp))
summary(lm(cons~income))
acf(income)

postscript("ice_cream_partial_residual.ps",height=5,width=6)
par(mfrow=c(2,2),cex.lab=1.5,cex.axis=1.5)
prplot(fit_ic_lm,1)
prplot(fit_ic_lm,2)
prplot(fit_ic_lm,3)
graphics.off()

av.plot(fit_ic_lm,"temp")

postscript("ice_cream_tsplots.ps",height=5,width=6)
par(mfrow=c(2,2))
plot(cons,type="b",main="",xlab="index")
plot(temp,type="b",main="",xlab="index")
plot(income,type="b",main="",xlab="index")
plot(price,type="b",main="",xlab="index")
graphics.off()

par(mfrow=c(1,2))
plot(residuals(fit_ic_ar))
plot(residuals(fit_ic_ma))





