library("Ecdat")
data(Earnings,package="Ecdat")
ind = (Earnings$age=="g1")
x = Earnings$y[ind]/1000
hist(x)

postscript("Earning_TKDE.ps",width=6,height=5)
f = density(x)
froot = density(sqrt(x))
ind2 = (froot$x > sqrt(min(x)))
plot(f$x[ind2],f$y[ind2],type="l",ylim=c(0,.035),xlim=c(0,100),
   ylab="Density(y)",xlab="y=income (in $1000)",lwd=2)
abline(h=0)
f2 = .5*froot$y / froot$x
lines(froot$x[ind2]^2, f2[ind2],type="l",
   ylim=c(0,.035),xlim=c(0,100),ylab="Density(y)",xlab="y=income (in $1000)",  
   main="TKDE",lty=2,lwd=2)
abline(h=0)
legend(60,.03,c("KDE","TKDE"),lty=c(1,2),lwd=2)
graphics.off()

postscript("Earning_hist.ps",width=7,height=4)
par(mfrow=c(1,2))
y=x
hist(y)
hist(sqrt(y))
graphics.off()