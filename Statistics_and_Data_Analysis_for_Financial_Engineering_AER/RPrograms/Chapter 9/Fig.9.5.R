#  Figure 9.5

set.seed(8716)
e = rnorm(200)
x1 = e
for (i in 2:200)
{
x1[i] = .98*x1[i-1]+e[i]
}
x2 = e
for (i in 2:200)
{
x2[i] = -.6*x2[i-1]+e[i]
}
x3 = e
x3 = cumsum(e)
x4 = e
for (i in 2:200)
{
x4[i] = 1.01*x4[i-1]+e[i]
}

postscript("ar_sim200.ps",height=6,width=6)  #  Figure 9.5
par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15,cex.main=1.15)

plot(x1,type="l",xlab="Time",ylab=expression(Y[t]),
   main=expression(paste(phi," = 0.98")))

plot(x2,type="l",xlab="Time",ylab=expression(Y[t]),
  main=expression(paste(phi == - 0.6)))

plot(x3,type="l",xlab="Time",ylab=expression(Y[t]),
   main=expression(paste(phi," = 1")))

plot(x4,type="l",,xlab="Time",ylab=expression(Y[t]),
   main=expression(paste(phi," = 1.01")))

graphics.off()

library(tseries)
adf.test(x1)
adf.test(x2)
adf.test(x3)
adf.test(x4)
adf.test(x4,alternative="exp")



