#  Figures 9.23 and 9.24

data(bmw,package="evir")
library(tseries)
bmw = as.vector(bmw)

postscript("BMW_pacf.ps",width=6,height=5)     #  Figure 9.23
pacf(bmw,main="BMW log returns")
graphics.off()

data(Mishkin,package="Ecdat")
infl = as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 

postscript("Inflation_pacf.ps",height=5,width=6)   #  Figure 9.24
pacf(diff(infl),main="Change in inflation rate")
graphics.off()
