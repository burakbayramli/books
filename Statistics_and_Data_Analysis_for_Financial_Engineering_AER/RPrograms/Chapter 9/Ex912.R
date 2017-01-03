#  Example 9.10

data(Mishkin,package="Ecdat")
library(forecast)

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 

auto.arima(x,d=0,ic="bic",stationary=T)
fitauto0 = arima(x,order=c(2,0,1))
polyroot( c(1,-fitauto0$coef[1:2]) )