#  Figure 9.8

data(Mishkin,package="Ecdat")

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 


year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)

fit = arima(x,c(1,0,0))

postscript("inflation_AR1_acf.ps",width=9,height=4.75) # Fig 9.8
par(mfrow=c(1,2))
acf(x,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")
graphics.off()


