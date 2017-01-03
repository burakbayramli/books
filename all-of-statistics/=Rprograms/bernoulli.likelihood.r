x <- 12
n <- 20
p <- seq(0,1,length=50)
l <- x*log(p) + (n-x)*log(1-p)
l <- l - max(l)
l <- exp(l)

postscript("bernoulli.likelihood.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,1))
plot(p,l,type="l",lwd=3,xlab="",ylab="")
l <- round(l,5)
print(cbind(p,l))
dev.off()

