data(SP500,package="Ecdat")
SPreturn = SP500$r500

postscript("SP_hist.ps")  #  Figure 4.4
par(mfrow=c(2,2))
hist(SPreturn,breaks=30,cex.lab=1.5,cex.axis=1.5,xlab="return",
   main="(a) 30 cells, full range",cex.main=1.5)
hist(SPreturn,breaks=30,main="(b) 30 cells, central range",cex.lab=1.5,
   cex.axis=1.5,xlim=c(-.04,.04),xlab="return",cex.main=1.5)
hist(SPreturn,breaks=20,main="(c) 20 cells, central range",cex.lab=1.5,
   cex.axis=1.5,xlim=c(-.04,.04),xlab="return",cex.main=1.5)
hist(SPreturn,breaks=50,main="(d) 50 cells, central range",cex.lab=1.5,
   cex.axis=1.5,xlim=c(-.04,.04),xlab="return",cex.main=1.5)
graphics.off()