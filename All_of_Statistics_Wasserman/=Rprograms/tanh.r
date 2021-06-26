m <- .5
theta <- seq(-m,m,length=100)
x <- seq(-5,5,length=1000)
risk <- rep(0,100)
tanh.fun <- function(z){
     return( (exp(z)-exp(-z))/(exp(z)+exp(-z)) )
     }
for(i in 1:100){
     f <- dnorm(x,theta[i],1)
     temp <- f*(theta[i]-m*tanh.fun(m*x))^2
     risk[i] <- sum(temp)
     }

postscript("tanh.eps",horizontal=F,onefile=F,print.it=F)
par(pty="s")
plot(theta,risk,type="l",lwd=3,xlab="",ylab="",axes=F)
axis(1,at=c(-.5,0,.5))
axis(2,at=c(5,10,15,20))
lines(c(-m,-m),c(min(risk),max(risk)/2),lwd=3)
points(-m,max(risk)/2,pch=20)
lines(c(m,m),c(min(risk),max(risk)/2),lwd=3)
points(m,max(risk)/2,pch=20)


dev.off()





