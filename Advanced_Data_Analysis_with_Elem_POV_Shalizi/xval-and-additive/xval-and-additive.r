#############################################################
#
# R code to accompany "cross validation and additive models"
# lecture...
#
#############################################################

#############################################################
#
# Interpolation splines vs smoothing splines
#

x <- rnorm(100)

y <- sin(pi*x) + rnorm(100,sd=.2)

plot(x,y)

lines(spline(x,y), method="natural", col="Red")  # interpolation spline

lines(smooth.spline(x,y),col="Green",lwd=2)


############################################################
#
# Choosing the smoothing parameter (spar is a monotone function of
# lambda...
#

for (sp in seq(.1,.9,length=9)) {
  lines(smooth.spline(x,y,spar=sp),lty=sp*10)
  scan()
}

############################################################
#
# Choosing spar by cross-validation, by hand
#

library(bootstrap) # install from cran!

fit <- function(x,y) {
  smooth.spline(x,y,spar=spar)
}

pred <- function(fit,x) {
  predict(fit,x)$y
}

spar <- .2

results <- crossval(x,y,fit,pred,ngroup=5)



mse <- NULL
sp <- seq(0.1,0.9,length=30)
for (spar in sp) {
 results <- crossval(x,y,fit,pred,ngroup=5) # try 10, 20, etc.
 mse <- c(mse,mean((y-results$cv.fit)^2))
}
plot(sp,mse)



############################################################
#
# Using smooth.spline's "leave one out" cross-validation
#

plot(x,y)

lines(smooth.spline(x,y,cv=T),col="Red",lwd=2)

smooth.spline(x,y,cv=T)$spar


############################################################
#
# Using smooth.spline's "generalized cross-validation"
#

lines(smooth.spline(x,y,cv=F),col="Green",lwd=2)

smooth.spline(x,y,cv=F)$spar


###########################################################
#
# Some demonstrations of gam()
#
##########################################################

# A regression example.

# install "gam" from CRAN
library(gam)


# Need MASS for "ppr"
library(MASS)

# This illustration follows loosely some of the examples in Venables
# and Ripley (MASS text), pp 232ff.

help(rock)
# 
#      A data frame with 48 rows and 4 numeric columns.
# 
#        [,1]  area   area of pores space, in pixels out of 256 by 256
#        [,2]  peri   perimeter in pixels
#        [,3]  shape  perimeter/sqrt(area)
#        [,4]  perm   permeability in milli-Darcies
# 
#      Twelve core samples from petroleum reservoirs were sampled by 4
#      cross-sections.  Each core sample was measured for permeability,
#      and each cross-section has total area of pores, total perimeter of
#      pores, and shape.
#
#      GOAL: predict log(perm)...

pairs(rock)

par(mfrow=c(2,2))
plot(lm.fit <- lm(log(perm) ~ .,data=rock))
summary(lm.fit)

gam.fit0 <- gam(log(perm) ~ s(area) + s(peri) + s(shape), data=rock)
summary(gam.fit0)
gam.fit1 <- gam(log(perm) ~ area + peri + s(shape), data=rock)
summary(gam.fit1)

anova(lm.fit,gam.fit1,gam.fit0)

par(mfrow=c(3,4))
plot.lm(lm.fit)
plot.lm(gam.fit1)
plot.lm(gam.fit0)

# in this case there doesn't seem to be much point in fitting anything
# but the linear model

par(mfrow=c(4,4))
plot.lm(lm.fit)
plot.lm(gam.fit1)
plot.lm(gam.fit0)
plot.lm(gam.fit2 <- gam(log(perm) ~ s(area,df=6) + s(peri,df=6) +
                        s(shape,df=6), data=rock)) 

par(mfrow=c(3,3))
plot(gam.fit2,se=T)
plot(gam.fit0,se=T)
plot(gam.fit1,se=T)




par(mfrow=c(4,3))
plot(gam.fit2,se=T)
plot(gam.fit0,se=T)
plot(gam.fit1,se=T)
plot(ppr.fit0 <- ppr(log(perm) ~ area + peri + shape,
                    data = rock, nterms = 3, max.terms = 5))

summary(ppr.fit0)

# Hard to see how each variable "contributes" to the projection
# pursuit directions, since the scales of the variables
# are so different...
#
# > round(apply(rock,2,sd),2)
#    area    peri   shape    perm 
# 2683.85 1431.66    0.08  437.82
#
# so we re-try with standardized variables...

X <- rock[,-4]
X <- sweep(X,2,apply(X,2,mean))
X <- sweep(X,2,apply(X,2,sd),"/")
X <- cbind(X,logperm=log(rock$perm))

par(mfrow=c(2,3))
plot(ppr.fit0)
plot(ppr.fit1 <- ppr(logperm ~ area + peri + shape,
                    data = X, nterms = 3, max.terms = 5))

summary(ppr.fit1)


######################################################################
#
# a second gam example...
#

# try this with both
#
# library(gam)
# library(mgcv)

set.seed(0) 
n<-400
sig<-2
x0 <- runif(n, 0, 1)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) 0*x
f <- f0(x0) + f1(x1) + f2(x2)
e <- rnorm(n, 0, sig)
y <- f + e

xx <- seq(0,1,length=100)

par(mfrow=c(2,2))

plot(xx,f0(xx),type="l")
plot(xx,f1(xx),type="l")
plot(xx,f2(xx),type="l")
plot(xx,f3(xx),type="l")

windows()

par(mfrow=c(2,2))
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3))
summary(b)
plot(b,pages=1,residuals=TRUE)





##########################################################
#
# Demonstrate bias-variance tradeoff for model complexity
#
##########################################################

plot.classifiers <- FALSE

# Generate some data...

x1 <- rnorm(100,1,.5)
x2 <- rnorm(100,2,.5)

y1 <- rnorm(100,1,.5)
y2 <- rnorm(100,2,.5)

y <- c(rep(-1,100),rep(1,100))
X <- cbind(c(x1,x2),c(y1,y2))

minx <- min(X[,1])
maxx <- max(X[,1])
xx <- seq(minx,maxx,length=50)

#####################################################################
 
# Fit LS polynomial classifiers of various complexity...

X.df <- data.frame(y,X)
X.df <- cbind(X.df,X12=X.df$X1^2,X22=X.df$X2^2)
X.df <- cbind(X.df,X13=X.df$X1^3,X23=X.df$X2^3)
X.df <- cbind(X.df,X14=X.df$X1^4,X24=X.df$X2^4)
X.df <- cbind(X.df,X15=X.df$X1^5,X25=X.df$X2^5)

train <- c(sample(1:100,25),sample(101:200,25))
test <- (1:200)[-train]

X.train <- X.df[train,]
X.test <- X.df[test,]

if (plot.classifiers) { par(mfrow=c(2,2)) }

results <- NULL
for (kmax in 0:5) {
  fla <- y~X1+X2
  if (kmax==0) { fla <- y ~ 1 }
      if (kmax>1) {
        fla <- formula(paste("y ~","(",
                             paste(names(X.df)[2:((kmax-1)*2+2)],collapse="+"),
                             ")^",kmax))
      }
      print(fla)
      fit <- lm(fla,X.train)
      print(fit)
      class <- ifelse(predict(fit,X.test)<0,-1,1)
      test.err <- sum((class-X.test[,1])^2)/length(class)
      class <- ifelse(predict(fit,X.train)<0,-1,1)
      train.err <- sum((class-X.train[,1])^2)/length(class)
      results <- rbind(results,c(k=kmax,train=train.err,test=test.err)) 

  if(plot.classifiers) {
    if (kmax>0 && kmax<4) {
      plot(X.test[,2:3],col=(X.test[,1]+1)/2+2,xlab="",ylab="")
      title(paste(kmax,"-degree polynomial",sep=""))
      X.pred <- X.train[1,]
      for (i in 1:length(xx)) {
        for (j in 1:length(xx)) {
          X.pred[1,2] <- xx[i]
          X.pred[1,3] <- xx[j]
          class <- ifelse(predict(fit,X.pred)<0,-1,1)
          points(xx[i],xx[j],col=(class+1)/2+2,pch=19,cex=.75)
        }
      }
    }
  }

}

results

matplot(results[,1],results[,2:3],type="l",lty=1:2,
        xlab="Degree of Polynomial Fit",
        ylab="Mean-Squared Error (MSE)")
legend(1,max(results[,"test"]),
       legend=c("MSE(train)","MSE(test)"),lty=1:2,col=1:2)
title("Training Error vs Test Error")
 

#############################################################
#
# Curse of dimensionality
#
# Suppose (x_1,y_1), ..., (x_n,y_n) are data we will use for
# estimating the function y=f(x).  We will estimate f(x) using
# a running average:
#
# Pick a "bandwidth" h, and let
#
#     C_h(x) = { all x_k : |x-x_k|<h }
#     N_h(x) = #C_h(x) (number of elements in C_h(x))
#
# Then we will estimate f(x) as
#
# f-hat(x) = (1/N_h(x)) * sum {y_k : x_k is in C_h(x)}
#
# [in other words, f-hat(x) is the average of all of the data
# within distance h of x]
#

fhat <- function(x,h) {
  Ch <- (0:n.data)[as.matrix(dist(rbind(x,x.data)))[,1]<h][-1]
  Nh <- length(Ch)
  val <- ifelse(Nh>0,mean(y.data[Ch]),NA)
  se <- ifelse(Nh>1,sd(y.data[Ch])/sqrt(Nh),NA)
  return(list(val=val,se=se,Nh=Nh,Ch=Ch))
}

h = 0.5

###############
#
# d=1...

n.data <- 64
x.data <- t(t(runif(n.data)))
y.data <- x.data^2

x <- runif(1)
fhat(x,h)

test <- fhat(x,h)
Ch <- test$Ch
Nh <- test$Nh

plot(c(0,1),c(0,1),type="n",xlab="",ylab="")
points(x.data,rep(0,n.data),pch="o",col="Red")
points(x.data[Ch,],rep(0,Nh),pch=19,col="Green",cex=1.5)
points(x,0,pch=19,col="Black",cex=1.5)

###############
#
# d=2...

n.data <- 64
x.data <- cbind(runif(n.data),runif(n.data))
y.data <- x.data[,1]^2 + x.data[,2]

x <- cbind(runif(1),runif(1))
fhat(x,h)

test <- fhat(x,h)
Ch <- test$Ch
Nh <- test$Nh

plot(c(0,1),c(0,1),type="n",xlab="",ylab="")
points(x.data[,1],x.data[,2],pch="o",col="Red")
points(x.data[Ch,1],x.data[Ch,2],pch=19,col="Green",cex=1.5)
points(x[,1],x[,2],pch=19,col="Black",cex=1.5)

###############
#
# d=3...

library(scatterplot3d)

n.data <- 64
x.data <- cbind(runif(n.data),runif(n.data),runif(n.data))
y.data <- x.data[,1]^2 + x.data[,2] - x.data[,3]^3

x <- cbind(runif(1),runif(1),runif(1))
fhat(x,h)

test <- fhat(x,h)
Ch <- test$Ch
Nh <- test$Nh

x.new <- rbind(x.data,x)
color <- rep("Red",n.data+1) ; pch <- rep(1,n.data+1)
color[Ch] <- "Green"         ; pch[Ch] <- 20
color[n.data+1] <- "Black"   ; pch[n.data+1] <- 20
scatterplot3d(x.new,color=color,pch=pch,cex.symbols=rep(1.5,n.data+1),
              xlab="",ylab="",zlab="")

##############
#
# generic d>3...
#

d <- 4

n.data <- 64
x.data <- matrix(runif(n.data*d),ncol=d)
y.data <- apply(x.data^2,1,sum)

x <- t(runif(d))
fhat(x,h)

##############

pts <- NULL
for (d in 1:10) {
  tot <- 0
  for (i in 1:100) {
    x.data <- matrix(runif(n.data*d),ncol=d)
    y.data <- apply(x.data^2,1,sum)
    x <- t(runif(d))
    tot <- tot + fhat(x,h)$Nh
  }
  avg <- tot/100
  pts <- c(pts,avg)
}
plot(1:length(pts),pts,xlab="Dimension",ylab="Number of points within h of x",cex=1)

###############
