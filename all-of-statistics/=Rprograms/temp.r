library(fields)

cv.exact.fun <- function(h,x,y){
     n <- length(x)
     nh <- length(h)
     cv <- rep(0,nh)
     risk <- rep(0,nh)
     temp <- rep(0,n)
     for(j in 1:nh){
          temp <- rep(0,n)
          for(i in 1:n){
               junk    <- nkreg(x[-i], y[-i], bandwidth=h[j],grid=x[i])$y
               temp[i] <- (junk - y[i])^2
               }
          risk[j] <- mean(temp,na.rm=T)
          }
     h.opt <- h[risk==min(risk)]
     return(risk=risk,h.opt=h.opt)
     }




cv.approx.fun <- function(h,x,y){
     o <- order(x)
     x <- x[o]
     y <- y[o]
     n    <- length(x)
     nh   <- length(h)
     cv   <- rep(0,nh)
     risk <- rep(0,nh)
     for(j in 1:nh){
          f <- nkreg(x, y, bandwidth=h[j],grid=x)$y
          w <- rep(0,n)
          for(i in 1:n){
               w[i] <- dnorm(0)/sum(dnorm((x[i]-x)/h[j]))
               }
          w <- 1/(1-w)^2
          risk[j] <- mean(w*(f-y)^2)
          }
     h.opt <- h[risk==min(risk)]
     return(risk=risk,h.opt=h.opt)
     }


###################################################



data1 <- scan("max_050801.dat")
data1 <- matrix(data1,ncol=4,byrow=T)
data2 <- scan("boom_050801.dat")
data2 <- matrix(data2,ncol=4,byrow=T)
data <- rbind(data1,data2)
x <- data[,1]
y <- data[,2]
data3 <- scan("dasi.dat");data3 <- matrix(data3,ncol=5,byrow=T)
x <- c(x,data3[,1])
y <- c(y,data3[,4])
o <- order(x)
x <- x[o]
y <- y[o]





#postscript("cmb.kernel.eps",horizontal=F,onefile=F,print.it=F)
#par(mfrow=c(2,2))
#plot(x,y,pch=19,xlab="",ylab="",sub="Undersmoothed",lwd=3)
#f <- nkreg(x, y, bandwidth=1,grid=x);lines(f,lwd=3)
#plot(x,y,pch=19,sub="Oversmoothed",xlab="",ylab="",lwd=3)
#f <- nkreg(x, y, bandwidth=200,grid=x);lines(f,lwd=3)
h     <- seq(20,120,length=100)
temp  <- cv.exact.fun(h,x,y)
h.opt <- temp$h.opt
#plot(x,y,pch=19,sub="Just Right (Using cross-valdiation)",xlab="",ylab="",lwd=3)
f <- nkreg(x, y, bandwidth=h.opt,grid=x);#lines(f,lwd=3)
#plot(h,temp$risk,type="l",lwd=3,xlab="bandwidth",ylab="estimated risk")
#dev.off()


#postscript("cmb.approx.cv.eps",horizontal=F,onefile=F,print.it=F)
#par(mfrow=c(1,1))
#temp  <- cv.exact.fun(h,x,y)
#plot(h,temp$risk,type="l",lwd=3,xlab="bandwidth",ylab="CV score")
#temp  <- cv.approx.fun(h,x,y)
#lines(h,temp$risk,type="l",lwd=3,lty=2)
#dev.off()
#

postscript("cmb.kernel.envelope.eps",horizontal=F,onefile=F,print.it=F)
h <- h.opt
n <- length(x)

sigma <- sqrt(sum((y[-n] - y[-1])^2)/(2*(n-1)))
k     <- 100
grid  <- seq(min(x),max(x),length=k)
f <- nkreg(x, y, bandwidth=h.opt,grid=grid)$y
sd    <- rep(0,k)
ess   <- sd
for(i in 1:k){
     w <- dnorm(grid[i],x,h)
     w <- w/sum(w)
     sd[i]  <- sqrt(sum(w^2))
     ess[i] <- sum(dnorm(grid[i],x,h))
     }
se <- sigma*sd
m <- (max(x) - min(x))/(3*h)
alpha <- .05
q <- qnorm( (1+ (1-alpha)^(1/m))/2)
u <- f + q*se
l <- f - q*se
Grid <- (grid[-1]+grid[-k])/2
Grid <- rep(Grid,rep(2,k-1))
Grid <- c(min(grid),Grid,max(grid))
U <- rep(u,rep(2,k))
L <- rep(l,rep(2,k))
plot(grid,u,type="l",lwd=4,xlab="",ylab="",ylim=c(-750,6000))
lines(grid,l,type="l",lwd=4)
polygon(c(Grid,rev(Grid)),c(L,rev(U)),density=25,col=4)
dev.off()



