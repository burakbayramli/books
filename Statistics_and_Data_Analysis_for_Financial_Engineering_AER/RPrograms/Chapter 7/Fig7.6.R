library(sn)
t = seq(-3,3,.025)
df=4
Omega = matrix(c(2,1.1,1.1,1),nrow=2)
ei = eigen(Omega)
O = ei$vectors

ff3 <- function(x,x2){dmst(cbind(x,x2),xi=c(0,0),
   df=df,alpha=c(0,0),Omega=Omega )}



postscript("bivariate_t.ps",width=5,height=5.65) # Fig 7.6

contour(t,t,outer(t,t,ff3),xlab=expression(X[1]),ylab=expression(X[2]),
 cex.lab=1,cex.axis=1,labcex=1,main="multivariate t",
drawlabels=T,cex.lab=1,cex.axis=1,
pin=c(3,5))

kk =5
lines(c(0,kk*O[1,1]),c(0,kk*O[2,1]),lwd=2)
kk=-5
lines(c(0,kk*O[1,1]),c(0,kk*O[2,1]),lwd=2)

kk =5
lines(c(0,kk*O[1,2]),c(0,kk*O[2,2]),lty=2,lwd=2)
kk=-5
lines(c(0,kk*O[1,2]),c(0,kk*O[2,2]),lty=2,lwd=2)
graphics.off()



