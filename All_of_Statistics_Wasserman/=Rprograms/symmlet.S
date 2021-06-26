module(wavelets)
postscript("symmlet.eps",horizontal=F,onefile=F,printit=F)

par(mfrow=c(2,1))
h <- wavelet("s8",mother=T)
plot(h,lwd=3,xlab="",ylab="")
h <- wavelet("s8",mother=F)
plot(h,lwd=3,xlab="",ylab="")

dev.off()

