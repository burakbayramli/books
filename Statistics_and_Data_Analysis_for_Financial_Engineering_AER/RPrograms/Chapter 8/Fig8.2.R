#  Figure 8.2

library(copula)

set.seed(543)
postscript("frank_copulas.ps",width=6,height=6)
par(mfrow=c(3,3),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)

y = rcopula(frankCopula(-100, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-100)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(-50, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-50)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(-10, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-10)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(-1, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-1)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(0, dim = 2),200)
plot(y,main=expression(paste(theta,"=",0)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(5, dim = 2),200)
plot(y,main=expression(paste(theta,"=",5)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(20, dim = 2),200)
plot(y,main=expression(paste(theta,"=",20)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(50, dim = 2),200)
plot(y,main=expression(paste(theta,"=",50)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(frankCopula(500, dim = 2),200)
plot(y,main=expression(paste(theta,"=",500)),
   xlab=expression(u[1]),ylab=expression(u[2]))
graphics.off()




