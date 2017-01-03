library("faraway")
library("car")
library("Ecdat")


data(Orange)
attach(Orange)

priceoj
producer price for frozen orange juice 
pricefg
producer price index for finished goods 
fdd
freezing degree days (from daily minimimum temperature reacorded at Orlando area airports) 
Source


fit_boxcox = boxcox(priceoj~pricefg+fdd,data=Orange)
summary(fit_boxcox)

fit_lm = lm(priceoj~pricefg+fdd,data=Orange)
par(mfrow=c(2,2))
qqnorm(fit_lm$resid)
plot(fit_lm$fitted,fit_lm$resid)
acf(fit_lm$resid)
summary(fit_lm)


fit_log_lm = lm(log(priceoj)~pricefg+fdd,data=Orange)
par(mfrow=c(2,2))

summary(fit_log_lm)


