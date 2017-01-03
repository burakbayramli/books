#  Figure 9.9


x1 = ARMAacf(ar=c(.5,-.3),lag.max=10)
x1 = as.vector(x1)

x2 = ARMAacf(ar=c(.5,.15),lag.max=10)
x2 = as.vector(x2)

x3 = ARMAacf(ar=c(.15,.8),lag.max=10)
x3 = as.vector(x3)

postscript("ar2acf.ps")        #  Figure 9.9
plot(x1,xlab="lag",ylab="acf",
  main= "ACF of three AR(2) processes",cex.axis=1.5,
  cex.lab=1.5,cex=2,cex.main=1.5,pch="*",type="b",ylim=c(-.5,1))
lines(x2,xlab="lag",ylab="acf",cex.axis=1.5,
  cex.lab=1.5,cex=2,pch="o",type="b")
lines(x3,xlab="lag",ylab="acf",cex.axis=1.5,
  cex.lab=1.5,cex=2,pch="x",type="b")
abline(h=0)
legend(6,-.1,c("(0.5, -0.3)", "(0.5, 0.15)","(0.15, 0.8)") ,
pch=c("*","o","x"),cex=1.5,box.lty=0 )
graphics.off()
