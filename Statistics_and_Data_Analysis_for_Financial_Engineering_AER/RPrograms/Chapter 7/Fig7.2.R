library("mnormt")
mu = rep(0,2)
cov1 = matrix(c(1,.5,.5,1),nrow=2)
cov2 = matrix(c(1,-.95,-.95,1),nrow=2)
t = seq(-2.5,2.5,.025)

ff1 <- function(x,x2){dmnorm(cbind(x,x2),mu,cov1)}
ff2 <- function(x,x2){dmnorm(cbind(x,x2),mu,cov2)}

postscript("bivariate_normal_contours.ps",width=8,height=4.5) # Fig 7.2

par(mfrow=c(1,2))

contour(t,t,outer(t,t,ff1),xlab=expression(X[1]),ylab=expression(X[2]),
 cex.lab=1,cex.axis=1,labcex=.8,main="(a) corr = 0.5" )

contour(t,t,outer(t,t,ff2),xlab=expression(X[1]),ylab=expression(X[2]),
 cex.lab=1,cex.axis=1,labcex=1,main="(b) corr = -0.95" ,
drawlabels=F)

graphics.off()

