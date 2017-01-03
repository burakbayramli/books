#  Figure 8.3

library(copula)

postscript("clayton_copulas.ps",width=7,height=7)

par(mfrow=c(3,3),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
set.seed(543)
y = rcopula(claytonCopula(-.98, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-0.98)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(-.7, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-0.7)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(-.3, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-0.3)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(-.1, dim = 2),200)
plot(y,main=expression(paste(theta,"=",-0.1)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(.1, dim = 2),200)
plot(y,main=expression(paste(theta,"=",.1)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(1, dim = 2),200)
plot(y,main=expression(paste(theta,"=",1)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(5, dim = 2),200)
plot(y,main=expression(paste(theta,"=",5)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(15, dim = 2),200)
plot(y,main=expression(paste(theta,"=",15)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(claytonCopula(100, dim = 2),200)
plot(y,main=expression(paste(theta,"=",100)),
   xlab=expression(u[1]),ylab=expression(u[2]))

graphics.off()