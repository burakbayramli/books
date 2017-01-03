#  Example 14.2 and Figure 14.3

phi = c(-.75,-.5,-.25,0,.25,.5,.75)
x = cbind(rep(1,21),seq(-10,10,1))
se = as.matrix(cbind(phi,rep(0,7),rep(0,7)))

for (i in 1:7)
{
xx = (t(x)%*%x)
xxinv = solve(xx)

sig = toeplitz(phi[i]^(0:20))

cov = xxinv %*% t(x) %*%sig %*% x %*% xxinv
se[i,2:3] = t(sqrt(diag(cov)))
}

postscript("example_arerrors.ps")  #  Figure 14.3
plot(se[,1],se[4,2]/se[,2],type="b",xlab=expression(phi),
  ylab= "SE ratio",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)
lines(se[,1],se[4,3]/se[,3],type="b",lty="dashed",pch="*",cex=1.5,lwd=2)
legend("topright",c("intercept","slope"),lty=c(1,2),pch=c("o","*"),
  lwd=2,cex=1.5)
graphics.off()