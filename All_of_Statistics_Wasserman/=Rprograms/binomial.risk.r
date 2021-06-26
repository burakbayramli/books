n <- 100
p <- seq(0,1,length=100)
postscript("binomial.risk.eps",horizontal=F,onefile=F,print.it=F)
par(pty="s")
r1 <- p*(1-p)/n
r2 <- rep(n/(4*(n+sqrt(n))^2),100)
plot(p,r1,type="l",xlab="",ylab="",lwd=3)
lines(p,r2,lty=2,lwd=3,col=2)
dev.off()




