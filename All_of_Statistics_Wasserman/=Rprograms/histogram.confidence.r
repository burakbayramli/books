###Confidence envelope for histogram



ci.fun <- function(x,alpha=.05,m=0){
     a <- min(x); b <- max(x)
     x <- (x-a)/(b-a)
     n <- length(x)
     if(m==0)m <- round(sqrt(n))
     c <- (qnorm(1-(alpha/m))/sqrt(2))*sqrt(m/n)
     br <- seq(0,1,length=m+1)
     h  <- 1/m
     p  <- hist(x,breaks=br,plot=F)$counts/n
     f  <- p/h
     u  <- (sqrt(f) + c)^2
     l  <- (pmax(sqrt(f) - c,0))^2
     Grid <- rep((1:(m-1))/m,rep(2,m-1))
     Grid <- c(0,Grid,1)
     Grid <- Grid*(b-a) + a
     f <- f/(b-a); l <- l/(b-a); u <- u/(b-a);
     U <- rep(u,rep(2,m))
     L <- rep(l,rep(2,m))
     F <- rep(f,rep(2,m))
     return(f=f,u=u,l=l,m=m,Grid=Grid,U=U,L=L,F=F)
     }





###galaxy data

x   <- scan("a1882_25.dat")
x   <- matrix(x,ncol=3,byrow=T)
ra  <- x[,1]
dec <- x[,2]
z   <- x[,3]
x   <- z
x   <- x[x <= 0.2]


postscript("redshift.hist.ci.eps",horizontal=F,onefile=F,print.it=F)




out <- ci.fun(x,m=73)
plot(out$Grid,out$U,type="l",xlab="",ylab="",lwd=4)
lines(out$Grid,out$L,lwd=4)
polygon(c(out$Grid,rev(out$Grid)),c(out$L,rev(out$U)),density=25,col=4)
dev.off()


