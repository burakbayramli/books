###########  regression with ARMA errors

library(AER)
data("USMacroG")
MacroDiff= as.data.frame(apply(USMacroG,2,diff))
attach(MacroDiff)
fit1 = arima(unemp,order=c(1,0,0),xreg=cbind(invest,government))

par(mfrow=c(2,2))
fit2= lm(unemp~invest+government+interest,data=MacroDiff)
summary(fit1)
postscript("RlabARMANoise.ps",width=7,height=3)
par(mfrow=c(1,2))
acf(fit1$res)
acf(fit2$res)
graphics.off()
AIC(fit1)
AIC(fit2)


fit3 = arima(unemp,order=c(2,0,0),xreg=cbind(invest,government))
fit4 = arima(unemp,order=c(1,0,1),xreg=cbind(invest,government))
AIC(fit1)
AIC(fit3)
AIC(fit4)
n=dim(MacroDiff)[1]
BIC_fit1 = AIC(fit1) + (log(n)-2)*2
BIC_fit2 = AIC(fit2) + (log(n)-2)*1
BIC_fit1
BIC_fit2


######  nonlinear regression  #########
library(Ecdat)
data(Irates)
r1 = Irates[,1]
n = length(r1)
lag_r1 = lag(r1)[-n]
delta_r1 = diff(r1)
n = length(lag_r1)
par(mfrow=c(3,2))
plot(r1,main="(a)")
plot(delta_r1,main="(b)")
plot(delta_r1^2,main="(c)")
plot(lag_r1,delta_r1,main="(d)")
plot(lag_r1,delta_r1^2,main="(e)")


#  CKLS (Chan, Karolyi, Longstaff, Sanders)

nlmod_CKLS = nls(delta_r1 ~ a * (theta-lag_r1),
   start=list(theta = 5,   a=.01),
   control=list(maxiter=200))
param = summary(nlmod_CKLS)$parameters[,1]
par(mfrow=c(2,2))
t = seq(from=1946,to =1991+2/12,length=n)
plot(lag_r1,ylim=c(0,16),ylab="rate and theta",
   main="(a)",type="l")
abline(h=param[1],lwd=2,col="red")

res_sq = residuals(nlmod_CKLS)^2
nlmod_CKLS_res <- nls(res_sq ~  A*lag_r1^B,
   start=list(A=.2,B=1/2) )
param2 = summary(nlmod_CKLS_res)$parameters[,1]
plot(lag_r1,sqrt(res_sq),pch=5,ylim=c(0,6),
   main="(b)")
attach(as.list(param2))
curve(sqrt(A*x^B),add=T,col="red",lwd=3)


nlmod_CKLS_wt = nls(delta_r1 ~ a * (theta-lag_r1),
   start=list(theta = 5,  a=.01),
   control=list(maxiter=200),
   weights=1/fitted(nlmod_CKLS_res))

plot(lag_r1,ylim=c(0,16),ylab="rate and theta",
   main="(c)",type="l")
param3 = summary(nlmod_CKLS_wt)$parameters[,1]
abline(h=param3[1],lwd=2,col="red")



##############  response transformation

library(AER)
data(HousePrices)

fit1 = lm(price~.,data=HousePrices)
summary(fit1)
library(MASS)
postscript("RlabBoxCox.ps",width=6,height=5)
fit2=boxcox(fit1,xlab=expression(alpha))
alpha = fit2$x[fit2$y==max(fit2$y)]
graphics.off()

library(car)
fit3=lm(box.cox(price,alpha)~.,data=HousePrices)
summary(fit3)
AIC(fit1)
AIC(fit3)


############  binary regression

library(AER)
data(HousePrices)
fit1 = glm(aircon~.,family="binomial",data=HousePrices)
summary(fit1)
library(MASS)
fit2 = stepAIC(fit1)
summary(fit2)

HousePrices[1,]

fit3 = glm(aircon~price+stories+gasheat,family="binomial")
summary(fit3)
AIC(fit2)
AIC(fit3)
dim(HousePrices)[1]

summary(fit3)
coef(fit3)

prob = plogis( c(1, 42000, 2, 0) %*% coef(fit3))
prob







