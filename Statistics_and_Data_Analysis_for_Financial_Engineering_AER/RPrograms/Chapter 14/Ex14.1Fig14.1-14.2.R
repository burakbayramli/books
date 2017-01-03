#  Example 14.1 and Figure 14.1 and 14.2

library("faraway")
library("car")
library("Ecdat")
data(Icecream)
attach(Icecream)

fit_ic_lm = lm(cons~income+price+temp)
summary(fit_ic_lm)
durbin.watson(fit_ic_lm)
fit_ic_ma = arima(cons,order=c(0,0,1),xreg=cbind(income,price,temp))
fit_ic_ar = arima(cons,order=c(1,0,0),xreg=cbind(income,price,temp))
options(digits=3)
print(fit_ic_ma,digits=5)
print(fit_ic_ar,digits=5)

postscript("ice_cream_acf_resid.ps",width=6.5,height=2.75)  #  Figure 14.1
par(mfrow=c(1,3))
acf(fit_ic_lm$resid,main="linear model/indep. noise",xlab="lag")
acf(fit_ic_ar$resid,main="linear model/AR(1) noise",xlab="lag")
acf(fit_ic_ma$resid,main="linear model/MA(1) noise",xlab="lag")
graphics.off()

postscript("ice_cream_tsplots.ps",height=5,width=6)  #  Figure 14.2
par(mfrow=c(2,2))
plot(cons,type="b",main="",xlab="index")
plot(temp,type="b",main="",xlab="index")
plot(income,type="b",main="",xlab="index")
plot(price,type="b",main="",xlab="index")
graphics.off()







