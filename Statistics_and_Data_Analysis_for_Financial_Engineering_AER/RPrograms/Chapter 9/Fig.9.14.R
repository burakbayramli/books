#  Figure 9.14


set.seed(4631)
y1 = arima.sim(n = 500, list(ar = c(0.4)))
y2= cumsum(y1)
y3 = cumsum(y2)

postscript("integratedSim.ps",width=5,height=6)   #  Figure 9.14
par(mfrow=c(3,1))
plot(y1,type="l",ylab=expression(y[1]),lwd=1)
plot(y2,type="l",xlab="Time",ylab=expression(y[2]),lwd=1)
plot(y3,type="l",xlab="Time",ylab=expression(y[3]),lwd=1)
graphics.off()