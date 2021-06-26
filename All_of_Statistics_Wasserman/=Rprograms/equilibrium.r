postscript("equilibrium1.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
n <- 1000
time <- (1:n)
x <- rep(0,n)
for(i in 2:n){
     y <- 2*rbinom(1,1,.5)-1
     x[i] <- x[i-1] + 5*y
     }
plot(time,x,type="l",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
dev.off()

postscript("equilibrium2.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
x <- rep(0,n)
x[1] <- 10
for(i in 2:n){
     proposal <- rnorm(1,x[i-1],1)
     ratio <- dnorm(proposal)/dnorm(x[i-1])
     u <- runif(1)
     if(u < ratio)x[i] <- proposal else x[i] <- x[i-1]
     }
plot(time,x,type="l",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
dev.off()

