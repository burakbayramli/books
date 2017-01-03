#  Example 17.3 and Figure 17.5

library("fEcofin")
data("equityFunds")
equityFunds[1:10,]
pairs(equityFunds[,2:9])
pcaEq = prcomp(equityFunds[,2:9])
summary(pcaEq)

postscript("equityFunds_scree.ps",width=7.2,height=3.5)  #  Figure 17.5
par(mfrow=c(1,2))
plot(pcaEq,main="(a)")
Names = names(equityFunds)[2:9]
plot(pcaEq$rotation[,1],type="b",ylab="PC",lwd=2,ylim=c(-1.4,2),main="(b)")
lines(pcaEq$rotation[,2],type="b",lty=2,lwd=2)
lines(pcaEq$rotation[,3],type="b",lty=3,lwd=2)
lines(0:9,0*(0:9))
legend("top",c("PC1","PC2","PC3"),lty=c(1,2,3),lwd=2,cex=.65)
text(4.35,-1.25, "   EASTEU   LATAM   CHINA   INDIA   ENERGY   MINING   GOLD   WATER",cex=.38)
graphics.off()
