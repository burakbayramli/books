#  Example 10.4 and Figures 10.7 and 10.8

library("fEcofin")

CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
IP = as.matrix(IP.dat$IP)[697:828,]  # 1977-01-31 to 1987-12-31
IP_diff1 = as.matrix(diff(log(IP)))
CPI_IP = as.data.frame(na.omit(cbind(CPI_diff1,IP_diff1)))
names(CPI_IP) = c("Delta CPI","Delta IP")

arFit1 = ar(CPI_IP,order.max=1)
arFit1

postscript("CPI_IP_ar1_resid_acf.ps",height=6,width=8)  # Figure 10.7
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
acf(na.omit(arFit1$resid))
graphics.off()

res = arFit1$resid[7:126,]
bPhi = arFit1$ar[,,]
bPhi2 = bPhi %*% bPhi
bPhi3 = bPhi2 %*% bPhi
bPhi4 = bPhi3 %*% bPhi

xxx = t(CPI_IP[131,])

forecasts =  xxx
means = (apply(CPI_IP,2,mean))
offset = xxx - means
for (i in 1:10)
{
offset = bPhi %*% offset
forecasts = cbind(forecasts,means + offset)
}

postscript("CPI_IP_AR_forecasts.ps",height=6,width=8)  #  Figure 10.8
plot(0:10,forecasts[2,],type="b",lty=2,ylim=c(0,.006),pch="*",cex=3,
   xlab="k",ylab="forecast",cex.axis=1.15,cex.lab=1.15,lwd=3)
lines(0:10,forecasts[1,],type="b",pch="o",cex=2,lwd=3)
abline(h=means[1])
abline(h=means[2],lty=2)
legend(2,.0015,c("CPI","IP"),lty=c(1,2),pch=c("o","*"),cex=1.6,lwd=3,
  pt.cex=c(2,3))
graphics.off()