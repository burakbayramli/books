n <- 100
x <- seq(0,1,length=n)
d <- x[2]-x[1]
m <- 6
Phi <- matrix(0,n,m)
Phi[,1] <- rep(1,n)
for(i in 2:m){
     Phi[,i] <- sqrt(2)*cos((i-1)*pi*x)
     }

postscript("cosine.basis.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(3,2))

for(i in 1:m){
     plot(x,Phi[,i],type="l",lwd=3,xaxt="n",yaxt="n",
         xlab="",ylab="")
     }
dev.off()

################################################################

postscript("cosine.approx.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))

n <- 2048
x <- seq(0,1,length=n)
d <- x[2]-x[1]
m <- 200
Phi <- matrix(0,n,m)
Phi[,1] <- rep(1,n)
for(i in 2:m){
     Phi[,i] <- sqrt(2)*cos((i-1)*pi*x)
     }

f <- sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
fhat <- rep(0,n)
a <- rep(0,m)

plot(x,f,type="l",lwd=2,xlab="",ylab="",xaxt="n",yaxt="n")

for(i in 1:m){
     a[i] <- d*sum(f*Phi[,i])
     fhat <- fhat + a[i]*Phi[,i]
     if( (i==5)|(i==20)|(i==200))plot(x,fhat,type="l",lwd=2,xlab="",ylab="",
                                 xaxt="n",yaxt="n")
     }

print(a)
dev.off()






