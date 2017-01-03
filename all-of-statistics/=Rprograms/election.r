x <- scan("florida-election-data.txt")
x <- matrix(x,ncol=22,byrow=T)
bush <- x[,3]
buchanan <- x[,4]

postscript("florida1.eps",horizontal=T,onefile=F,print.it=F)
plot(bush,buchanan,xlab="",ylab="",xaxt="n",yaxt="n",pch=16)
axis(1,c(0,125000,250000),cex.axis=2)
axis(2,c(0,1500,3000),cex.axis=2)
temp <- lm(buchanan ~ bush)
abline(temp,lwd=3)
dev.off()

postscript("florida2.eps",horizontal=T,onefile=F,print.it=F)
r <- temp$residuals
plot(bush,r,xlab="",ylab="",xaxt="n",yaxt="n",pch=16,ylim=c(-500,500))
axis(1,c(0,125000,250000),cex.axis=2)
axis(2,c(-500,0,500),cex.axis=2)
lines(c(0,max(bush)),c(0,0),lty=2,lwd=3)
dev.off()

postscript("florida3.eps",horizontal=T,onefile=F,print.it=F)
bush <- log(bush)
buchanan <- log(buchanan)
plot(bush,buchanan,xlab="",ylab="",xaxt="n",yaxt="n",pch=16,xlim=c(7,13))
axis(1,7:13,cex.axis=2)
axis(2,2:8,cex.axis=2)
temp <- lm(buchanan ~ bush)
abline(temp,lwd=3)
dev.off()

postscript("florida4.eps",horizontal=T,onefile=F,print.it=F)
r <- temp$residuals
plot(bush,r,xlab="",ylab="",xaxt="n",yaxt="n",pch=16,xlim=c(7,13),ylim=c(-1,1))
axis(1,7:13,cex.axis=2)
axis(2,c(-1,0,1),cex.axis=2)
lines(7:13,rep(0,7),lty=2,lwd=3)
dev.off()

