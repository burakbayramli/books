#  Figure 8.4

library(copula)

set.seed(543)
postscript("gumbel_copulas.ps",width=6,height=5)

par(mfrow=c(2,3),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)

y = rcopula(gumbelCopula(1.1, dim = 2),200)
plot(y,main=expression(paste(theta,"=",1.1)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(gumbelCopula(1.5, dim = 2),200)
plot(y,main=expression(paste(theta,"=",1.5)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(gumbelCopula(2, dim = 2),200)
plot(y,main=expression(paste(theta,"=",2)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(gumbelCopula(4, dim = 2),200)
plot(y,main=expression(paste(theta,"=",4)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(gumbelCopula(8, dim = 2),200)
plot(y,main=expression(paste(theta,"=",8)),
   xlab=expression(u[1]),ylab=expression(u[2]))

y = rcopula(gumbelCopula(50, dim = 2),200)
plot(y,main=expression(paste(theta,"=",50)),
   xlab=expression(u[1]),ylab=expression(u[2]))

graphics.off()