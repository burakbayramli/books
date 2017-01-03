#  Figures 5.13-5.16

library(fGarch)
data(Garch,package="Ecdat")
attach(Garch)
data(Capm,package="Ecdat")
y=diff(Capm$rf)
diffrf=y
stdFit(y)

x1 = pstd(y,mean = 0.001, sd = .0725, nu = 3.34)
x = qnorm(x1)

postscript("diffrf_TKDE.ps",width=6,height=5) # Fig 5.13
d1 = density(diffrf)
plot(d1$x,d1$y,type="l",xlab="y",ylab="Density(y)")
d2 = density(x)
ginvx = qstd(pnorm(d2$x), mean = 0.001, sd = .0725, nu = 3.34)
gprime_num = dstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)
gprime_den = dnorm(qnorm(pstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)))
gprime = gprime_num/gprime_den
lines(ginvx,d2$y*gprime,type="l",lty=2)
legend("topleft",c("KDE","TKDE"),lty=c(1,2),lwd=2,cex=1.5)
graphics.off()

postscript("diffrf_TKDE_detail.ps",width=6,height=5) # Fig 5.14
d1 = density(diffrf)
plot(d1$x,d1$y,type="l",xlab="y",ylab="Density(y)",xlim=c(-.5,-.1),
   ylim=c(0,1))
d2 = density(x)
ginvx = qstd(pnorm(d2$x), mean = 0.001, sd = .0725, nu = 3.34)
gprime_num = dstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)
gprime_den = dnorm(qnorm(pstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)))
gprime = gprime_num/gprime_den
lines(ginvx,d2$y*gprime,type="l",lty=2)
legend("topleft",c("KDE","TKDE"),lty=c(1,2),lwd=2,cex=1.5)
graphics.off()

postscript("diffrf_TKDE_trans.ps",width=6,height=5) # Fig 5.15
ygrid  = seq(min(y),max(y),.01)
plot(ygrid, qnorm(pstd(ygrid,mean = 0.001, sd = .0725, nu = 3.34)  ),
   type="l",lwd=2,xlab="y",ylab="g(y)")
graphics.off()

postscript("diffrf_TKDE_QQ.ps",width=6,height=5)  # Fig 5.16
qqnorm( qnorm(pstd(jitter(y,factor=2),mean = 0.001, sd = .0725, nu = 3.34)),
  datax=T )
graphics.off()

