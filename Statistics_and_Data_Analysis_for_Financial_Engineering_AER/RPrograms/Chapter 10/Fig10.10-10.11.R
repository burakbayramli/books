library("fracdiff")
library("longmemo")

D = c(-.35, .35)

postscript("FARIMA_sim.ps",height=6,width=5)  #  Figure 10.10
set.seed("09201948")
par(mfrow=c(length(D)+1,2))
for (i in 1:length(D))
{
H = D[i] + 1/2
x = simARMA0(2500,H)
plot(x,main=toString(paste("d =", D[i]))  )
 acf(x,main=toString(paste("d =", D[i]))  )
}

d= .7-1
H = d + 1/2
y = simARMA0(2500,H)
x = cumsum(y)
plot(x,main="d = 0.7",type="l")
acf(x,main="d = 0.7")
graphics.off()

postscript("FARIMA_sim_diff_ACF.ps",width=6,height=3)  #  Figure 10.11
par(mfrow=c(1,2))
acf(diffseries(x,.7),main = expression(paste(Delta^0.7, Y)))
acf(diff(x),main = expression(paste(Delta, Y)))
graphics.off()


