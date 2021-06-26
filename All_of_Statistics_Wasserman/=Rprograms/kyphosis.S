postscript("kyphosis1.ps",horizontal=F)
par(mfrow=c(2,2))

print(names(kyphosis))
x <- kyphosis$Age
n <- length(x)
k <- kyphosis$Kyphosis
y <- rep(0,n)
y[k=="present"] <- 1
plot(x,y,pch=17,xlab="Age",ylab="Disease")
print(range(x))
print(summary(x))

x0 <- x[y==0]
x1 <- x[y==1]
w <- sum(y)/length(y)
w <- .5

library(MASS)

h0 <- ucv(x0)
temp <- density(x0,from=1,to=206,n=1000,width=h0,window="gaussian")
f0 <- temp$y
xgrid <- temp$x
plot(xgrid,f0,type="l",xlab="Age",ylab="f0")
h1 <- ucv(x1)
temp <- density(x1,from=1,to=206,n=1000,width=h1,window="gaussian")
f1 <- temp$y
plot(xgrid,f1,type="l",xlab="Age",ylab="f1")
bayesrule <- w*f1/( w*f1 + (1-w)*f0 )
plot(xgrid,bayesrule,type="l",xlab="Age",ylab="Pr(Y=1|Age)",ylim=c(0,1))
lines(xgrid,rep(.5,length(xgrid)))
temp <- xgrid[bayesrule > .5]
print(range(temp))

####try other bandwidths
postscript("kyphosis2.ps",horizontal=F)
par(mfrow=c(2,2))
plot(x,y,pch=17,xlab="Age",ylab="Disease")
temp <- density(x0,from=1,to=206,n=1000,width=h0/2,window="gaussian")
f0 <- temp$y
plot(xgrid,f0,type="l",xlab="Age",ylab="f0")
temp <- density(x1,from=1,to=206,n=1000,width=h1/2,window="gaussian")
f1 <- temp$y
plot(xgrid,f1,type="l",xlab="Age",ylab="f1")
bayesrule <- w*f1/( w*f1 + (1-w)*f0 )
plot(xgrid,bayesrule,type="l",xlab="Age",ylab="Pr(Y=1|Age)",ylim=c(0,1))
lines(xgrid,rep(.5,length(xgrid)))
temp <- xgrid[bayesrule > .5]
print(range(temp))

postscript("kyphosis3.ps",horizontal=F)
par(mfrow=c(2,2))
plot(x,y,pch=17,xlab="Age",ylab="Disease")
temp <- density(x0,from=1,to=206,n=1000,width=2*h0,window="gaussian")
f0 <- temp$y
plot(xgrid,f0,type="l",xlab="Age",ylab="f0")
temp <- density(x1,from=1,to=206,n=1000,width=2*h1,window="gaussian")
f1 <- temp$y
plot(xgrid,f1,type="l",xlab="Age",ylab="f1")
bayesrule <- w*f1/( w*f1 + (1-w)*f0 )
plot(xgrid,bayesrule,type="l",xlab="Age",ylab="Pr(Y=1|Age)",ylim=c(0,1))
lines(xgrid,rep(.5,length(xgrid)))
temp <- xgrid[bayesrule > .5]
print(range(temp))




