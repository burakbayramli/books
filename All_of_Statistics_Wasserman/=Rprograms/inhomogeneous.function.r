postscript("inhomogeneous.function.eps",horizontal=F,onefile=F,print.it=F)
x <- seq(0,1,length=1000)
#f <- 1 + x + x^2 + dnorm(x,.5,.01)/50
f <- dnorm(x,.75,.5) + dnorm(x,.5,.01)/50;
plot(x,f,type="l",lwd=3,xlab="",ylab="")
dev.off()
