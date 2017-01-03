#  Figure 5.5

postscript("NormMix01.ps",height=6,width=6)
par(mfrow=c(2,2))
x=seq(-6,15,by=.01)
plot(x,dnorm(x,sd=sqrt(3.4)),type="l",lwd=2,ylim=c(0,.4),
   xlab="x",ylab="density",main="(a) densities")
lines(x,.9*dnorm(x)+.1*dnorm(x,sd=5),lwd=2,lty=5)
legend("topright",c("normal","mixture"),lwd=2,lty=c(1,5))

plot(x,dnorm(x,sd=sqrt(3.4)),type="l",lwd=2,ylim=c(0,.025),xlim=c(4,15),
   xlab="x",ylab="density",main="(b) densities")
lines(x,.9*dnorm(x)+.1*dnorm(x,sd=5),lwd=2,lty=5)
legend("topright",c("normal","mixture"),lwd=2,lty=c(1,5))

set.seed("7953")
y1 = rnorm(200,sd=sqrt(3.4))
qqnorm(y1,datax=T,main="(c) QQ plot, normal",xlab="theoretical quantiles",
ylab="sample quantiles")
qqline(y1,datax=T)

#rbinom(n, size, prob)

n2=rbinom(1,200,.9)
y2 = c(rnorm(n2),rnorm(200-n2,sd=5))
qqnorm(y2,datax=T,main="(d) QQ plot, mixture",xlab="theoretical quantiles",
ylab="sample quantiles")
qqline(y2,datax=T)
graphics.off()