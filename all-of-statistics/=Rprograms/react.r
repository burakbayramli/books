react.fun <- function(y,alpha=.05,J=0){
     n <- length(y)
     x <- (1:n)/n
     k <- round(n/4)
     z <- rep(0,n)
     z[1] <- mean(y)
     for(i in 2:n){
          z[i] <- mean(y*sqrt(2)*cos( (i-1)*pi*x ))
          }
     temp <- z[(n-k+1):n]
     sigma <- sqrt(n/k) * sqrt(sum( temp^2) )
     risk <- rep(0,n)
     for(i in 1:(n-1)){
          r <- i
          risk[i] <- (r*sigma^2/n) + sum(z[(r+1):n]*z[(r+1):n] - (sigma^2/n) )
          }
     risk[n] <- sigma^2
     if(J==0)J <- ((1:n)[risk==min(risk)])[1]

     a <- rep(0,length(x))
     f <- rep(z[1],n)
     for(i in 2:J){
          phi <- sqrt(2)*cos( (i-1)*pi*x )
          a   <- a + phi^2
          f <- f + z[i]*phi
          }
     a <- sqrt(a)

#%     tau   <- sqrt(2*J*sigma^4*(1 + (J/k))/n )
#%     L.hat <- (J*sigma^2/n)
#%     K <- sqrt(2)
#%
#%     d <- L.hat + (tau*qnorm(1-alpha)/sqrt(n))
#%     a <- K*sqrt(J)*sqrt(d)
     c   <- a*sigma*sqrt(qchisq(1-alpha,J))/sqrt(n)
     up  <- f + c
     low <- f - c
     list(x=x,f=f,risk=risk,sigma=sigma,
          up=up,low=low,J=J)
     }



n <- 2048
x <- seq(0,1,length=n)
f <- sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
y <- f + rnorm(n,0,.1)


postscript("doppler1.eps",horizontal=F,onefile=F,print.it=F)
out <- react.fun(y)
print(out$J)
par(mfrow=c(1,1))
#plot(x,f,type="l",lwd=3,xlab="",ylab="",xaxt="n",yaxt="n")
plot(x,y,xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
dev.off()
postscript("doppler2.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
plot(x,out$f,type="l",lwd=3,xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
#plot(1:n,out$risk,type="l",lwd=3,xlab="J",ylab="",xaxt="n",yaxt="n")
dev.off()

postscript("doppler1.ci.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
a <- min(out$low)
b <- max(out$up)
plot(x,out$f,type="l",lwd=3,xlab="",ylab="",
     ylim=c(a,b),xaxt="n",yaxt="n",bty="l")
lines(x,out$low,lwd=3)
lines(x,out$up,lwd=3)
dev.off()

postscript("doppler2.ci.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
J <- round(sqrt(n))
out <- react.fun(y,alpha=.05,J)
plot(x,out$f,type="l",lwd=3,xlab="",ylab="",
     ylim=c(a,b),xaxt="n",yaxt="n",bty="l")
lines(x,out$low,lwd=3)
lines(x,out$up,lwd=3)
dev.off()






