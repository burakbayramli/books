###density confidence interval


library(MASS)

###geyser data 
postscript("density.ci.ps",horizontal=F)
x <- geyser$waiting
n <- length(x)
a <- 45;b <- 95
h <- ucv(x)
h <- h/n^(2/15)
out <- density(x,window="gaussian",width=h,from=a,to=b,n=100)
f <- out$y
xgrid <- out$x
plot(xgrid,f,type="l",xlab="waiting time",ylab="density",ylim=c(0,.08))

B <- 1000
tt <- rep(0,B)
for(i in 1:B){
     xx <- sample(x,replace=T)
     out <- density(xx,window="gaussian",width=h,from=a,to=b,n=100)
     fstar <- out$y
     tt[i] <- max(abs( (fstar-f)/sqrt(fstar) ))
     }
critical <- quantile(tt,.95)
lower <- f - critical*sqrt(f)
upper <- f + critical*sqrt(f)
lower[lower<0] <- 0

lines(xgrid,lower,lty=3)
lines(xgrid,upper,lty=3)

