library(chron)
x <- read.table("www.data")
junk <- x[,1]
junk <- as.character(junk)
temp <- x[,2]
temp <- as.character(temp)
y <- chron(dates=junk,times=temp,format=c("d/m/y","h:m:s"))
n <- length(y)
d <- difftime(y[2:n],y[1:(n-1)])
w <- cumsum(as.vector(d))
postscript("poisson.process1.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1),pin=c(6,3))
###plot(w[1:20]/60,rep(1,20),type="h",xlab="time",ylab="")
one <- rep(1,length(w))
plot(w/60,one,type="h",xlab="",ylab="",xaxt="n",yaxt="n",bty="n",fin=c(1,1))
axis(1,at=c(0,400,800,1200),cex.axis=2)
dev.off()

postscript("poisson.process2.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(1,1))
###plot(y[1:20],1:20,type="s",xlab="time (seconds)",ylab="X(t)")
plot(y,1:n,type="s",xlab="",ylab="")
N <- length(y)
lambda.hat <- N/as.vector(difftime(y[N],y[1]))
print(lambda.hat)
dev.off()

postscript("poisson.process3.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2))
hist(w/60)
plot(density(w/60),xlab="")
plot(density(w/60,bw="ucv"),xlab="")
dev.off()

tt <- max(w)
I1 <- sum(w <= (tt/4))
I2 <- sum( (w > (tt/4))&(w <= (tt/2)) )
I3 <- sum( (w > (tt/2))&(w <= (3*tt/2)) )
I4 <- sum( (w > (3*tt/4)))
n <- I1 + I2 + I3 + I4
p1 <- .25;p2 <- .25;p3 <- .25;p4 <- .25;
chi2 <- (I1-n*p1)^2/(n*p1) + (I2-n*p2)^2/(n*p2) +
        (I3-n*p3)^2/(n*p3) + (I4-n*p4)^2/(n*p4)
print(chi2)
print(1-pchisq(chi2,3))

