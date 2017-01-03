library("Ecdat")
library("lmtest")
library("faraway")

data(Money)
Money=as.data.frame(Money)
attach(Money)
n = dim(Money)[1]
Money = cbind(Money[2:n,],m[1:n-1])
Money = na.omit(Money)
attach(Money)
names(Money) = c("logMoney","logGPD","logPrice","rate","laglogMoney")
fit1 =lm(logMoney~laglogMoney+logGPD+logPrice+rate)
fit2=lm(logMoney~laglogMoney)
summary(fit1)
summary(fit2)
dwtest(fit1,alternative="two.sided")
par(mfrow=c(3,3))
acf(fit1$resid)
cr.plot(fit1,"logGPD")
cr.plot(fit1,"logPrice")
cr.plot(fit1,"laglogMoney")
halfnorm(abs(fit1$resid))
plot(fit1$resid)
plot(fit2$resid)

boxcox(m~.,data=Money)



m
log of the real money supply 
y
the log of GDP, in 1992 dollars, seasonally adjusted 
p
the log of the price level 
r
the 3-month treasury till rate 
Source