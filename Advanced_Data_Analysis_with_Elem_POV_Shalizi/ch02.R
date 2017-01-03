# R code for examples in the chapter "The Truth about Linear Regression"

# Figure 1
x1 = runif(100)
x2 = rnorm(100,0.5,0.1)
x3 = runif(100,2,3)
y1 = sqrt(x1) + rnorm(100,0,0.05)
y2 = sqrt(x2) + rnorm(100,0,0.05)
y3 = sqrt(x3) + rnorm(100,0,0.05)
plot(x1,y1,xlim=c(0,3),ylim=c(0,3))
rug(x1,side=1)
rug(y1,side=2)
points(x2,y2,pch=24,col="blue")
rug(x2,side=1,col="blue")
rug(y2,side=2,col="blue")
points(x3,y3,pch=22,col="red")
rug(x3,side=1,col="red")
rug(y3,side=2,col="red")
lm1 = lm(y1 ~ x1)
lm2 = lm(y2 ~ x2)
lm3 = lm(y3 ~ x3)
# abline takes intercept and slope as its first two arguments...
abline(lm1$coefficients)
abline(lm2$coefficients,col="blue")
abline(lm3$coefficients,col="red")
x.all=c(x1,x2,x3)
y.all=c(y1,y2,y3)
lm.all = lm(y.all~x.all)
abline(lm.all$coefficients,lty=2)
curve(sqrt(x),col="grey",add=TRUE)


# Figure 2: Make the 3D plot to show omitted variable bias
library(lattice)
library(MASS)  # for multivariate normal generator

# Make correlated normal variables X and Z
x.z = mvrnorm(100,c(0,0),matrix(c(1,0.1,0.1,1),nrow=2))
# Y = X+Z + small noise
y = x.z[,1] + x.z[,2] + rnorm(100,0,0.1)
# 3D scatterplot
cloud(y~x.z[,1]*x.z[,2],xlab="X",ylab="Z",zlab="Y")


# Figure 3
# Change the correlation between X and Z to -0.1 instead of +0.1
new.x.z = mvrnorm(100,c(0,0),matrix(c(1,-0.1,-0.1,1),nrow=2))
new.y = new.x.z[,1] + new.x.z[,2] + rnorm(100,0,0.1)
cloud(new.y~new.x.z[,1]*new.x.z[,2],xlab="X",ylab="Z",zlab="Y")

# Figure 4
# Now omit Z and plot
plot(x.z[,1],y,xlab="x",xlim=range(c(x.z[,1],new.x.z[,1])),ylim=range(c(y,new.y)))
# Make sure the range encompasses both data sets!
rug(x.z[,1],side=1)
axis(y,side=2)
points(new.x.z[,1],new.y,col="blue")
rug(new.x.z[,1],side=1,col="blue")
rug(new.y,side=2,col="blue")
# ... and regress
old.lm = lm(y ~ x.z[,1])
new.lm = lm(new.y ~ new.x.z[,1])
abline(old.lm$coefficients)
abline(new.lm$coefficients,col="blue")

# Figure 5
x <- runif(100)
y <- rnorm(100,mean=log(x),sd=1)
plot(y~x)
curve(log(x),add=TRUE,col="grey")
abline(lm(y~x))


