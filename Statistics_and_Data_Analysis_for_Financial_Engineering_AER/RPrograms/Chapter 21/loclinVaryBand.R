library("KernSmooth")
set.seed("9988")

scale=.25
x = seq(0,1,length=75)

true = .1*x+ sin(5* x^1.5) + 3.6

y = true + rnorm(75,sd=.25)

h1 = dpill(x,y)
fit1 <- locpoly(x,y, bandwidth = h1)
fit2 <- locpoly(x,y, bandwidth = 3*h1)
fit3 <- locpoly(x,y, bandwidth = h1/3)

options(digits=4)
muhat1 = spline(fit1$x,fit1$y,xout=x)$y
mean((true-muhat1)^2)

muhat2 = spline(fit2$x,fit2$y,xout=x)$y
mean((true-muhat2)^2) / mean((true-muhat1)^2)

muhat3 = spline(fit3$x,fit3$y,xout=x)$y
mean((true-muhat3)^2) / mean((true-muhat1)^2)


postscript("loclinVaryBand.ps",width=6,height=5)
plot(x,y,ylim=c(1.5,5),cex=.5,pch="o")
lines(fit1$x,fit1$y,lwd=5)
lines(fit2$x,fit2$y,lty=2,lwd=3)
lines(fit3$x,fit3$y,lty=1,lwd=2)
#lines(x,true,col="red",lwd=3)
legend("bottomleft",c("dpi","3*dpi","dpi/3"),lty=c(1,2,1),lwd=c(5,3,2))
graphics.off()