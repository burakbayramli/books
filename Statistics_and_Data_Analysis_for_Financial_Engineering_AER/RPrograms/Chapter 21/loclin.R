set.seed("9988")
library("KernSmooth")
scale=.25
x = seq(0,1,length=75)
true = .1*x+ sin(5* x^1.5) + 3.6
y = true + rnorm(75,sd=.2)
h = dpill(x,y)
w1 = scale*dnorm(x,mean=x[25],sd=h)
fit1 = lm(y~x,weights=w1)
w2 = scale*dnorm(x,mean=x[55],sd=h)
fit2 = lm(y~x,weights=w2)

fit3 <- locpoly(x,y, bandwidth = h)

postscript("loclin.ps",width=6,height=5)
plot(x,y,ylim=c(0,6),cex=1.15,pch="*")
lines(x,w1,lty="dashed",lwd=2)
lines(x,w2,lty="dashed",lwd=2)
lines(x[12:38],fit1$fitted[12:38],lwd=3)
lines(x[42:68],fit2$fitted[42:68],lwd=3)
points(c(x[25],x[55]),c(fit1$fit[25],fit2$fit[55]),pch="+",cex=4)
lines(fit3$x,fit3$y,lwd=3,lty=1)
abline(h=0,lwd=2)
graphics.off()

