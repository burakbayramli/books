set.seed(997711)
n = 200

par(mfrow=c(2,2))
x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit1= lm(y~x)
fit5 = lm(diff(y)~diff(x))
plot(x,y,type="p")

x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit2= lm(y~x)
fit6 = lm(diff(y)~diff(x))
plot(x,y,type="p")


x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit3= lm(y~x)
fit7 = lm(diff(y)~diff(x))
plot(x,y,type="p")


x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit4= lm(y~x)
fit8 = lm(diff(y)~diff(x))
plot(x,y,type="p")

 summary(fit1)$coefficients
 summary(fit2)$coefficients
 summary(fit3)$coefficients
 summary(fit4)$coefficients


 summary(fit5)$coefficients
 summary(fit6)$coefficients
 summary(fit7)$coefficients
 summary(fit8)$coefficients

