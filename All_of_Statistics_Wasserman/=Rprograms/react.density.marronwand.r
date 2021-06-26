int.fun <- function(f,x){
     n <- length(x)
     h <- diff(x)
     .5*sum(h*(f[-1]+f[-n]))
     }


react.fun <- function(x,grid,k=0,surgery=FALSE){
     n <- length(x)
     if(k==0)k <- round(n^(2/5))
     z <- rep(0,k)
     z[1]     <- 1
     sigma    <- rep(0,k)
     sigma[1] <- 0
     for(i in 2:k){
          temp     <- sqrt(2)*cos( (i-1)*pi*x)
          z[i]     <- mean(temp)
          sigma[i] <- sqrt(var(temp))
          }
     risk <- rep(0,k)
     for(i in 1:(k-1)){
          I <- 1:i
          J <- ((i+1):k)
          risk[i] <- (1/n)*sum((sigma[I])^2) + 
                     sum(pmax((z[J])^2 - (sigma[J])^2/n,0))
          }
     risk[k] <- sum(sigma^2)/n

     J <- ((1:k)[risk==min(risk)])[1]
     f <- rep(z[1],length(grid))
     for(i in 2:J){
          phi <- sqrt(2)*cos( (i-1)*pi*grid )
          f <- f + z[i]*phi
          }
     ### confidence radius     
     c <- 2*sqrt(J/n)*sqrt(qchisq(.95,J))
     if(surgery==TRUE)f[f<0] <- 0;      f <- f/int.fun(f,grid)
     u <- f+c
     l <- f-c
     if(surgery==TRUE)l[l<0] <- 0
     list(grid=grid,f=f,risk=risk,sigma=sigma,k=k,l=l,u=u)
     }








mix.dens.fun <- function(w,mu,sigma,grid){
   ###generate density for a mixture of normals
   k <- length(mu)
   out <- rep(0,length(grid))
   for(i in 1:k){
     out <- out+w[i]*dnorm(6*grid-3,mu[i],sigma[i])
     }
   return(6*out)
   }


mix.ran.fun <- function(w,mu,sigma,n){
   ###generate n observations from a mixture of normals
   ### rescale from [-3,3] to [0,1]
   k <- length(mu)
   out <- rnorm(n*k,rep(mu,rep(n,k)),rep(sigma,rep(n,k)))
   out <- matrix(out,n,k)
   index <- rep(1,n);w <- cumsum(w)
   u <- runif(n)
   for(i in 2:k){
      index[ (u>w[i-1]) & (u<=w[i])] <- i
      }
   x <- rep(0,n)
   for(i in 1:n){
      x[i] <- out[i,index[i]]
      }
   x[x<-3] <- -3
   x[x>3]  <- 3
   (x-min(x))/(max(x)-min(x))
   }


ww <- list( 1, c(1,1,3)/5, rep(1/8,8), c(2,1)/3, c(1,9)/10,
        c(1,1)/2,   c(1,1)/2,   c(3,1)/4, c(9,9,2)/20,
        c(5,1,1,1,1,1)/10, c(49/100,49/100,rep(1/350,7)),
        c(1/2,8/31,4/31,2/31,1/31,.5/31),
        c(46/100,46/100,1/300,1/300,1/300,7/300,7/300,7/300),
        2^(5-(0:5))/63, c(rep(2/7,3),rep(1/21,3)) )

mu <- list( 0, c(0,1/2,13/12), 3*((2/3)^(0:7)-1), c(0,0),
         c(0,0), c(-1,1), c(-3/2,3/2), c(0,3/2),
         c(-6/5, 6/5, 0), c(0, (0:4)/2 -1),
         c(-1,1, ((0:6)-3)/2), c(0, (-2:2)+.5),
         c(2*(0:1)-1,-(1:3)/2,(1:3)/2),
         c( (65-96*(1/2)^(0:5))/21), c( (12*(0:2)-15)/7,2*(8:10)/7))

sigma <- list( 1, c(1,2/3,5/9), (2/3)^(0:7) , c(1,1/10),
            c(1,1/10), c(2/3,2/3), c(1/2,1/2),
            c(1,1/3), c(3/5,3/5,1/4), c(1,rep(1/10,5)),
            c(2/3,2/3,rep(.01,7)), c(1,2^c(2,1,0,-1,-2)/10),
            c(2/3,2/3,.01,.01,.01,.07,.07,.07),
            (32/63)/2^(0:5), c(2/7,2/7,2/7,1/21,1/21,1/21) )


################################################################





#postscript("temp.eps",horizontal=F,onefile=F,print.it=F)
postscript("temp.ps",horizontal=F)
grid <- seq(0,1,length=100)
n <- 5000;
par(mfrow=c(2,1))
for(i in 1:15){

print(i)

x <- mix.ran.fun(ww[[i]],mu[[i]],sigma[[i]],n)


out <- react.fun(x,grid)
plot(grid,mix.dens.fun(ww[[i]],mu[[i]],sigma[[i]],grid),type="l",lwd=3,
     xlab="",ylab="",ylim=c(0,max(out$u)))
k <- length(grid)
Grid <- (grid[-1]+grid[-k])/2
Grid <- rep(Grid,rep(2,k-1))
Grid <- c(min(grid),Grid,max(grid))
u <- out$u;l <- out$l
U <- rep(u,rep(2,k))
L <- rep(l,rep(2,k))
plot(grid,u,type="l",lwd=4,xlab="",ylab="",ylim=c(0,max(u)))
lines(grid,l,type="l",lwd=4)
lines(out$grid,out$f,lwd=3)
polygon(c(Grid,rev(Grid)),c(L,rev(U)),density=25,col=4)
}

dev.off()



####galaxy data
#
#x   <- scan("a1882_25.dat")
#x   <- matrix(x,ncol=3,byrow=T)
#ra  <- x[,1]
#dec <- x[,2]
#z   <- x[,3]
#x   <- z
#x   <- x[x <= 0.2]
#x <- (x-min(x))/(max(x)-min(x))
#
#print(length(x))
#postscript("redshift.react.eps",horizontal=F,onefile=F,print.it=F)
#par(mfrow=c(2,2))
#
#out <- react.fun(x,grid,k=10)
#plot(out$grid,out$f,type="l",lwd=3,xlab="",ylab="",ylim=c(0,max(out$u)))
#lines(out$grid,out$u);lines(out$grid,out$l)
#
#out <- react.fun(x,grid,k=20)
#plot(out$grid,out$f,type="l",lwd=3,xlab="",ylab="",ylim=c(0,max(out$u)))
#lines(out$grid,out$u);lines(out$grid,out$l)
#
#out <- react.fun(x,grid,k=30)
#plot(out$grid,out$f,type="l",lwd=3,xlab="",ylab="",ylim=c(0,max(out$u)))
#lines(out$grid,out$u);lines(out$grid,out$l)
#
#out <- react.fun(x,grid,k=40)
#plot(out$grid,out$f,type="l",lwd=3,xlab="",ylab="",ylim=c(0,max(out$u)))
#lines(out$grid,out$u);lines(out$grid,out$l)
#
#
##plot(1:out$k,log(out$risk),xlab="",ylab="",type="l",lwd=3)
#dev.off()
#
