#  Example 14.10 and Figure 14.15

x= seq(-1,2.5,.025)
n = length(x)
set.seed(3592)
y = exp(-x) + rnorm(n,sd=.06)

postscript("linearizing_sim.ps",width=6,height=5)  #  Figure 14.15
par(mfrow=c(2,2),cex.axis=1.05,cex.lab=1.05)
plot(x,y,main="(a)")
plot(x,log(y),main="(b)")
fit = lm(log(y)~x)
qqnorm(fit$res,datax=T,main="(c)",ylab="sample quantiles",xlab="theoretical
quantiles")
qqline(fit$res,datax=T)
plot(fit$fit,fit$res,xlab="fitted value",ylab="residual",main="(d)")
graphics.off()

summary(fit)