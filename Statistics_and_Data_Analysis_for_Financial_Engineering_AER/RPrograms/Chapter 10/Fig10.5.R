#  Figure 10.5

data(Hstarts,package="Ecdat")
library("forecast")
library("FitAR")
Hstart_noLog = exp(Hstarts[,1])

Hstart.arima = arima(Hstart_noLog,c(1,1,1),
   seasonal = list(order = c(1,1,1), period = 4))

postscript("Hstart_BoxCox.ps",width=6,height=5)  #  Figure 10.5
BoxCox.Arima(Hstart.arima)
graphics.off()


