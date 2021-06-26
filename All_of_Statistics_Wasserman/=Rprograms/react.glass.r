react.fun <- function(x,y){
     n <- length(y)
     o <- order(x)
     x <- x[o]
     y <- y[o]
     savex <- x
     x <- (1:n)/n
     k <- round(n/4)
     z <- rep(0,n)
     z[1] <- mean(y)
     for(i in 2:n){
          z[i] <- mean(y*sqrt(2)*cos( (i-1)*pi*x ))
          }
     sigma <- sqrt(n)*sqrt(  sum(z[(n-k+1):n]*z[(n-k+1):n])/k )
     risk <- rep(0,n)
     for(i in 1:(n-1)){
          r <- i
          risk[i] <- (r*sigma^2/n) + sum(z[(r+1):n]*z[(r+1):n] - (sigma^2/n) )
          }
     risk[n] <- sigma^2
     ### fit
     r <- ((1:n)[risk==min(risk)])[1]
     f <- rep(z[1]*mean(y),n)
     for(i in 2:r){
          phi <- sqrt(2)*cos( (i-1)*pi*x )
          f <- f + z[i]*phi
          }
     ### confidence radius     
     J <- c(rep(1,r),rep(0,n-r))
     I <- c(rep(0,n-k),rep(1,k))
     h1 <- (2*J-1)  +   I*((1/k)*sum(1-2*J))
     h2 <- (J-1)    +   I*((1/k)*sum(1-2*J))

     tau2 <- 2*sigma^4*mean(h1^2) + max(4*sigma^2*mean((z^2-sigma^2)*h2^2),0)
     tau <- sqrt(tau2)
     Risk <- risk[r]
     d <- Risk + tau*qnorm(.95)/sqrt(n)
     d <- sqrt(d) 
     up  <- f + d*sqrt(2)
     low <- f - d*sqrt(2)

     list(x=savex,f=f,risk=risk,tau=tau,d=d,sigma=sigma,
          Risk=Risk,up=up,low=low,r=r)
     }

postscript("react.glass.ps",horizontal=F)
par(mfrow=c(2,1))
library(MASS)
data(fgl)
x <- fgl[[4]]
y <- fgl[[1]]
plot(x,y,cex=.5,pch=19)
out <- react.fun(x,y)
lines(out$x,out$f,col=1,lwd=2)
lines(out$x,out$low,col=2)
lines(out$x,out$up,col=2)
plot(1:length(x),out$risk,type="l",
     xlab="",ylab="Estimated Risk")

dev.off()






