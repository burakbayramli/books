#  Example 10.6 and Figure 10.12

data(Mishkin,package="Ecdat")
library("fracdiff")
library("forecast")
x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)

fit.frac = fracdiff(x,nar=0,nma=0)
summary(fit.frac)

fdiffx = diffseries(x,.4)

postscript("inflation_fractionalDiff_acf.ps",width=6,height=5)  #  Figure 10.12
par(mfrow=c(2,2))
acf(x,main="d = 0")
acf(fdiffx,main="d = 0.4")
acf(diff(x),main="d = 1")
graphics.off()

auto.arima(fdiffx,ic="bic")





