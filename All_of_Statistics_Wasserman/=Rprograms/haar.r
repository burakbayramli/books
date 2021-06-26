mother <- function(j,k,n){
     x <- seq(0,1,length=n)
     psi <- rep(0,n)
     y <- (2^j)*x - k
     I <- (1:n)[ (0 <= y) & (y <= .5) ]
     psi[I] <- -2^(j/2)
     I <- (1:n)[ (.5 < y) & (y <= 1) ]
     psi[I] <- 2^(j/2)
     psi
     }


postscript("haar.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))
n <- 1000
x <- seq(0,1,length=n)
phi <- rep(1,n)
plot(x,phi,type="l",lwd=3,ylim=c(-4,4),xlab="",ylab="")
j <- 0; k <- 0;psi <- mother(j,k,n)
plot(x,psi,type="s",lwd=3,xlab="",ylab="",ylim=c(-4,4))
j <- 2; k <- 2;psi <- mother(j,k,n)
plot(x,psi,type="s",lwd=3,xlab="",ylab="",ylim=c(-4,4))
j <- 4; k <- 10;psi <- mother(j,k,n)
plot(x,psi,type="s",lwd=3,xlab="",ylab="",ylim=c(-4,4))
dev.off()


############### doppler
int <- function(f,x){
     n <- length(x)
     h <- diff(x)
     .5*sum(h*(f[-1]+f[-n]))
     }


reconstruct <- function(n,J){
     x <- seq(0,1,length=n)
     f <- sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
     alpha <- int(f,x)
     g <- rep(alpha,n) ### reconstruction
     beta <- list()
     for(j in 0:J){
          m <- (2^j) - 1
          beta[[j+1]] <- rep(0,m+1)
          for(k in 0:m){
               a1 <- k/(2^j)
               a2 <- (k+.5)/(2^j)
               a3 <- (k+1)/(2^j)
               nn <- round(n/2)
               x1 <- seq(a1,a2,length=nn)
               x2 <- (seq(a2,a3,length=nn+1))[-1]
               f1 <- sqrt(x1*(1-x1))*sin(2.1*pi/(x1+.05))
               f2 <- sqrt(x2*(1-x2))*sin(2.1*pi/(x2+.05))
               temp <- 2^(j/2)*(int(f2,x2) - int(f1,x1))
               beta[[j+1]][k+1] <- temp
               psi <- mother(j,k,n)
               g <- g + temp*psi
               }
          }
     list(g=g,alpha=alpha,beta=beta)
     }



postscript("haar.doppler.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))
n <- 1024
x <- seq(0,1,length=n)
f <- sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
plot(x,f,type="l",lwd=2)

out <- reconstruct(n,3);plot(x,out$g,type="l",lwd=2,xlab="",ylab="")
out <- reconstruct(n,5);plot(x,out$g,type="l",lwd=2,xlab="",ylab="")
out <- reconstruct(n,8);plot(x,out$g,type="l",lwd=2,xlab="",ylab="")




dev.off()



############## Estimation

universal <- function(y){
     n <- length(y)
     J <- log(n,2)
     alpha <- mean(y)
     temp <- y/sqrt(n)
     D <- list()
     for(j in (J-1):0){
         m <- 2^j
         I <- (1:m)
         D[[j+1]] <- (temp[2*I] -  temp[(2*I) - 1])/sqrt(2)
         temp <- (temp[2*I] +  temp[(2*I) - 1])/sqrt(2)
         }
     sigma <- sqrt(n)*median(abs(D[[J]]))/.6745
     thresh <- sigma*sqrt(2*log(n)/n)
     beta <- D
     for(j in (J-1):0){
         temp <- D[[j+1]]
         temp[abs(temp) < thresh] <- 0
         beta[[j+1]] <- temp
         }
     x <- (1:n)/n
     f <- rep(alpha,n)
     for(j in 0:(J-1)){
          m <- (2^j) - 1
          for(k in 0:m){
               psi <- mother(j,k,n)
               f <- f + beta[[j+1]][k+1]*psi
               }
          }
     list(alpha=alpha,beta=beta,D=D,f=f)
     }

postscript("haar.doppler.estimate.eps",horizontal=F,onefile=F,print.it=F)
n <- 2048
x <- seq(0,1,length=n)
f <- sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
y <- f + rnorm(n,0,.1)

out <- universal(y)
par(mfrow=c(2,1))
plot(x,y,xlab="",ylab="")
plot(x,out$f,type="l",lwd=3,xlab="",ylab="")
dev.off()


