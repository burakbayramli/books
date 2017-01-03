#  Example 20.2 and Figure 20.1

p1 = .6^5
p2 = .4^5
numer = p1
denom = p1 + p2
p1
p2
numer
denom
numer/denom

x=seq(0,1,.01)
postscript("bayes_densities.ps",width=6,height=5) #  Figure 20.1
plot(x,dbeta(x,7,2),type="l",lwd=2,
  ylab=expression(paste("density(",theta,")")),xlab=expression(theta))
lines(x,dbeta(x,2,2),lty=5,lwd=2)
abline(v=qbeta(.05,7,2),lty=5)
abline(v=qbeta(.95,7,2),lty=5)
abline(v=6/7,lty="dotted")
abline(h=0)
legend("topleft",c("prior","posterior"),lwd=2,lty=c(5,1))
graphics.off()