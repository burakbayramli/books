### empirical cdf figures 
x <- scan("~/=all_of_statistics/=hand.data/datasets/nerve.dat")
n <- length(x)
x <- sort(x)

postscript("edf.eps",horizontal = FALSE, onefile = FALSE, paper ="special",
           width=6,height=6)
par(pin=c(5,3))
plot(sort(x),(1:n)/n,type="s",lwd=3,ylim=c(0,1),xlim=c(0,1.5),
     xlab="",ylab="",yaxt="n",xaxt="n",bty="n")
axis(1,c(0.0,0.5,1.0,1.5),cex.axis=2)
axis(2,c(0.0,0.5,1.0),cex.axis=2)
alpha <- .05
eps <- sqrt(log(2/alpha)/(2*n))
print(eps)
F.hat <- (1:n)/n
upper <- pmin(F.hat + eps,1)
lower <- pmax(F.hat - eps,0)
lines(sort(x),upper,type="s",lwd=2,col=2,lty=2)
lines(sort(x),lower,type="s",lwd=2,col=2,lty=2)

rug(x,ticksize=.1)
dev.off()

#
#
#plot(x,rep(1,n),ylim=c(0,1.2),xlim=c(0,1.5),
#     xlab="",ylab="",yaxt="n",xaxt="n",bty="n")
#axis(1,c(0.0,0.5,1.0,1.5),cex.axis=2)
#for(i in 1:n){
#     lines(c(x[i],x[i]),c(0,1))
#     }
#
#
#
#postscript("edf2.eps",horizontal = FALSE, onefile = FALSE, paper ="special",
#	    width=6,height=6)
#dev.off()
#
#
