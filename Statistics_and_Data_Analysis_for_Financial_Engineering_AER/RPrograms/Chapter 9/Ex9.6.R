#  Example 9.6

data(Mishkin,package="Ecdat")

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 


year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)

auto.arima(diff(x),max.p=20,max.q=0,ic="aic")

auto.arima(diff(x),max.p=20,max.q=0,ic="bic")