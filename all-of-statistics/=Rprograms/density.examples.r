###Some histogram and kernel density estimate examples in R


cv.hist.fun <- function(x){
     ### histogram cross validation function
     n <- length(x)
     a <- min(x)
     b <- max(x)
     k <- 100
     nbins <- seq(1,n,length=k)  ###number of bins
     nbins <- round(nbins)
     h <- (b-a)/nbins            ###width of bins
     risk <- rep(0,k)
     for(i in 1:k){
          ###get counts N_j
          br <- seq(a,b,length=nbins[i]+1)
          N <- hist(x,breaks=br,plot=F)$counts
          risk[i] <- sum(N^2)/(n^2*h[i])  - (2/(h[i]*n*(n-1)))*sum(N*(N-1))
          }
     hbest <- h[risk==min(risk)]
     hbest <- hbest[1]  ###in case of tie take first (smallest) one
     mbest <- (b-a)/hbest   ###optimal number of bins
     list(risk=risk,nbins=nbins,h=h,mbest=mbest)
     }

toothpick.plot.fun <- function(x,xlab=""){
     ### make a toothpick plot of the data x
     plot(x,rep(1,length(x)),type="h",xlab=xlab,ylab="",ylim=c(0,2))
     return(0)
     }



toothpick.fun <- function(x,a=1){
     for(i in 1:length(x)){
          lines(c(x[i],x[i]),c(0,a))
          points(c(x[i],x[i]),c(0,a))
          }
     return(0)
     }

bw.fun <- function (x, nb = 1000,lower=0,upper=0){
  n <- length(x)
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  if(upper==0)upper <- hmax;  
  if(lower==0)lower <- 0.1*hmax
  fucv <- function(h, x, n, d) .C("band_ucv_bin", as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1), PACKAGE = "base")$u
  if (!is.numeric(x) || !length(x)) stop("invalid x")
  storage.mode(x) <- "double"
  Z <- .C("band_den_bin", as.integer(n), as.integer(nb), d = double(1), 
        x, cnt = integer(nb), PACKAGE = "base")
  d <- Z$d
  cnt <- as.integer(Z$cnt)
  h <- optimize(fucv, c(lower, upper), tol = 0.1 * lower, x = cnt, 
        n = n, d = d)$minimum
  if (h < 1.1 * lower | h > upper - 0.1 * lower) 
      warning("minimum occurred at one end of the range")
    h.vec <- seq(lower, upper, length=200)
    u.vec <- rep(NA, 200)
    for(i in 1:200)  u.vec[i] <- fucv(h.vec[i], x=cnt, n, d)
    list(h.optimal=h, bandwidths=h.vec, risk=u.vec)
    }




############################################################


###galaxy data histograms

x   <- scan("a1882_25.dat")
x   <- matrix(x,ncol=3,byrow=T)
ra  <- x[,1]
dec <- x[,2]
z   <- x[,3]
x   <- z
x   <- x[x <= 0.2]


postscript("redshift.hist.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))
temp <- cv.hist.fun(x)
hist(x,nclass=10,xlab="Oversmoothed",ylab="",freq=F,main="")
hist(x,nclass=73,xlab="Just Right",ylab="",freq=F,main="",ylim=c(0,60))
hist(x,nclass=1000,xlab="Undersmoothed",ylab="",freq=F,main="")
plot(temp$nbins,temp$risk,type="l",
     xlab="number of bins",ylab="cross validation score")
mbest <- temp$mbest
print(mbest)
dev.off()


######## Kernel
postscript("redshift.kernel.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))
library(MASS)
n     <- length(x)
temp  <- bw.fun(x+rnorm(n,0,.004))
h     <- temp$h.optimal
hgrid <- temp$bandwidths
risk  <- temp$risk
f <- density(x,width=10*h,from=min(x),to=max(x),n=100)
plot(f,type="l",xlab="",ylab="",main="",lwd=2,xaxt="n",yaxt="n",bty="l")
axis(1,c(0.0,0.1,0.2),cex.axis=2)
f <- density(x,width=h,from=min(x),to=max(x),n=100)
plot(f,type="l",xlab="",ylab="",main="",lwd=2,ylim=c(0,70),xaxt="n",yaxt="n",bty="l")
axis(1,c(0.0,0.1,0.2),cex.axis=2)
f <- density(x,width=.01*h,from=min(x),to=max(x),n=100)
plot(f,type="l",xlab="",ylab="",main="",lwd=2,xaxt="n",yaxt="n",bty="l")
axis(1,c(0.0,0.1,0.2),cex.axis=2)
m <- round(length(hgrid)/2)
plot(hgrid[1:m],risk[1:m],type="l",xlab="",ylab="",lwd=2,xaxt="n",yaxt="n",bty="l")
axis(1,c(0.002,0.004,0.006),cex.axis=2)
dev.off()






postscript("redshift.kernel.envelope.eps",horizontal=F,onefile=F,print.it=F)
n     <- length(x)
k     <- 100
f     <- density(x,width=h,from=min(x),to=max(x),n=k)$y
grid  <- seq(min(x),max(x),length=k)
sd    <- rep(0,k)
ess   <- sd
for(i in 1:k){
     temp   <- dnorm(grid[i],x,h)/sqrt(n)
     sd[i]  <- sqrt(var(temp))
     ess[i] <- sum(dnorm(grid[i],x,h))
     }
m   <- (max(x)-min(x))/(3*h)
alpha <- .05
q <- qnorm( (1+ (1-alpha)^(1/m))/2)
u <- f + q*sd
l <- pmax(f - q*sd,0)
Grid <- (grid[-1]+grid[-k])/2
Grid <- rep(Grid,rep(2,99))
Grid <- c(min(grid),Grid,max(grid))
U <- rep(u,rep(2,k))
L <- rep(l,rep(2,k))
plot(grid,u,type="l",lwd=4,xlab="",ylab="")
lines(grid,l,type="l",lwd=4)
polygon(c(Grid,rev(Grid)),c(L,rev(U)),density=25,col=4)
dev.off()


