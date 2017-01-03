library(sn)
t = seq(-2.5,2.5,.025)
df=4

ff3 <- function(x,x2){dmst(cbind(x,x2),xi=c(0,0),
   df=df,alpha=c(-1,.25),Omega=diag(c(1,1)) )}


postscript("bivariate_skew_t.ps",width=5,height=5)

contour(t,t,outer(t,t,ff3),xlab=expression(X[1]),ylab=expression(X[2]),
 cex.lab=1,cex.axis=1,labcex=1,main=expression( paste("multivariate skewed t:  ",
   alpha[1]," = -1, ", 
   alpha[2]," = 0.25") ),
drawlabels=T,cex.lab=1,cex.axis=1)
abline(h=-2,lty=2)
abline(h=0,lty=2)
abline(h=2,lty=2)
abline(v=-2,lty=2)
abline(v=0,lty=2)
abline(v=2,lty=2)

graphics.off()

