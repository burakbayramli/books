#  Example 18.6 and Figure 18.9

library(fGarch)
data(bmw,package="evir")
n = length(bmw)
t=attr(bmw,"time")
year = 1973 + (1996 + 7/12 - 1973)*(1:n)/n
origin1 = 4100
t[origin1]
origin2 = 3880
nahead = 1500
bmw.garch_t_1 = garchFit(~garch(1,1),cond.dist="std",data=bmw[1:origin1])
pred1 = predict(bmw.garch_t_1,n.ahead=nahead)
coef1 = as.numeric(coef(bmw.garch_t_1))
t1=qstd(.975, mean = 0, sd = 1, nu = coef1[5])

bmw.garch_t_2 = garchFit(~garch(1,1),cond.dist="std",data=bmw[1:origin2])
pred2 = predict(bmw.garch_t_2,n.ahead=nahead)
coef2 = as.numeric(coef(bmw.garch_t_2))
t2=qstd(.975, mean = 0, sd = 1, nu = coef2[5])

postscript("bmw_garchForecast.ps",height=5,width=6)  #  Figure 18.9
lwd1 = 4
plot(year,bmw,type="l",
   xlab="year",ylab="return",xlim=c(1986,1992),ylim=c(-.13,.21),
   main="Forecasting BMW returns")
lines(year[(origin1+1):(origin1+nahead)],pred1$meanForecast+t1*pred1$standardDeviation,
   col="black",lwd=lwd1)
lines(year[(origin1+1):(origin1+nahead)],pred1$meanForecast-t1*pred1$standardDeviation,
   col="black",  lwd=lwd1)
lines(year[(origin2+1):(origin2+nahead)],pred2$meanForecast+t2*pred2$standardDeviation,
   col="black",  lwd=lwd1,lty=2)
lines(year[(origin2+1):(origin2+nahead)],pred2$meanForecast-t2*pred2$standardDeviation,
   col="black",  lwd=lwd1,lty=2)
legend("topleft",c("11-15-87","9-18-88"),lty=c(2,1),lwd=lwd1,bty="n")
graphics.off()