library("faraway")
library("car")
library("Ecdat")

e = arima.sim(n=1250,list(ar=.7,ma=.2),sd = .2)
x = rnorm(1250)
y = x + e
fit01=arima0(y,order=c(1,0,1),xreg=x)






data(Icecream)
fit_ic_lm = lm(cons~income+price+temp,data=Icecream)
summary(fit_ic_lm)
Box.test(fit_ic_lm$resid)

durbin.watson(fit_ic_lm)
acf(fit_ic_lm$resid)
attach(Icecream)
fit_ic_ma = arima0(cons,order=c(0,0,1),xreg=cbind(income,price,temp))
fit_ic_ar = arima0(cons,order=c(1,0,0),xreg=cbind(income,price,temp))
options(digits=3)
print(fit_ic_ma,digits=5)
print(fit_ic_ar,digits=5)

postscript("ice_cream_acf_resid.ps")
par(mfrow=c(1,3),cex.axis=1.75,cex.lab=1.75,cex.main=1.75)
acf(fit_ic_lm$resid,main="linear model/indep. noise")
acf(fit_ic_ar$resid,main="linear model/AR(1) noise")
acf(fit_ic_ma$resid,main="linear model/MA(1) noise")
graphics.off()

summary(lm(cons~temp))
summary(lm(cons~income+temp))
summary(lm(cons~income))
acf(income)

postscript("ice_cream_partial_residual.ps")
par(mfrow=c(2,2),cex.lab=1.5,cex.axis=1.5)
prplot(fit_ic_lm,1)
prplot(fit_ic_lm,2)
prplot(fit_ic_lm,3)
graphics.off()

postscript("ice_cream_tsplots.ps",height=13,width=8)
par(cex.axis=1.5,cex.lab=1.5,mfrow=c(4,1))
plot(cons,type="b",main="")
plot(temp,type="b",main="")
plot(income,type="b",main="")
plot(price,type="b",main="")
graphics.off()

par(mfrow=c(1,2))
plot(residuals(fit_ic_ar))
plot(residuals(fit_ic_ma))





