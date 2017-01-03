#  Figure 10.9

library("fEcofin")
library("MASS")
options(digits=5)

CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
CPI_diff2 = as.matrix(diff(log(CPI),diff=2))
n = length(CPI)
year = as.vector(1977 + (1987-1977)*(1:n)/n)
IP2 = as.matrix(IP.dat$IP)[697:828,]  # 1977-01-31 to 1987-12-31
IP_diff1 = as.matrix(diff(log(IP2)))
CPI_IP = as.data.frame(na.omit(cbind(CPI_diff1,IP_diff1)))
names(CPI_IP) = c("Delta CPI","Delta IP")
means = (apply(CPI_IP,2,mean))

arFit1 = ar(CPI_IP,order.max=1)
bPhi = arFit1$ar[,,]
bSigma = arFit1$var.pred

niter = 50000
fore_CPI = matrix(0,nrow=niter,ncol=11)
fore_IP = fore_CPI
for (k in 1:niter)
{
forecasts =  t(CPI_IP[131,])
for (i in 1:11)
{
fore_CPI[k,i] = forecasts[1]
fore_IP[k,i] = forecasts[2]
fore_means = means + bPhi %*% (forecasts-means)
forecasts = mvrnorm(n=1,mu=fore_means,Sigma=bSigma)
}
}

fore_CPI_xbar=apply(fore_CPI,2,mean)
fore_IP_xbar=apply(fore_IP,2,mean)
plot(0:10,fore_IP_xbar,type="b",lty=2,ylim=c(0,.006),pch="*",cex=3,
   xlab="k",ylab="forecast",cex.axis=1.5,cex.lab=1.5,lwd=3)

lines(0:10,fore_CPI_xbar,type="b",pch="o",cex=2,lwd=3)
abline(h=means[1])
abline(h=means[2],lty=2)
legend(2,.0015,c("IP","CPI"),lty=c(2,1),pch=c("*","o"),cex=1.6,lwd=3,
  pt.cex=c(3,2))

ul_CPI = 0*(1:11)
ll_CPI =ul_CPI
mean_CPI=ul_CPI
for (k in 1:11)
{
ul_CPI[k] = quantile(fore_CPI[,k],.975)
ll_CPI[k] = quantile(fore_CPI[,k],.025)
mean_CPI[k] = mean(fore_CPI[,k])
}

ul_IP = 0*(1:11)
ll_IP =ul_IP
mean_IP=ul_IP
for (k in 1:11)
{
ul_IP[k] = quantile(fore_IP[,k],.975)
ll_IP[k] = quantile(fore_IP[,k],.025)
mean_IP[k] = mean(fore_IP[,k])
}

postscript("CPI_IP_forecasts_sim.ps")  #  Figure 10.9
par(mfrow=c(2,1))
plot(0:10,ul_CPI,type="b",lty=2,ylim=c(-0.003,.013),lwd=2,
   xlab="lag",ylab="forecast",cex.axis=1.5,cex.lab=1.5,
   main="CPI",cex=1.5)
lines(0:10,ll_CPI,type="b",lty=2,lwd=2,cex=1.5)
lines(0:10,mean_CPI,type="b",lwd=2,pch="*",cex=1.5)
plot(0:10,ul_IP,,type="b",lty=2,ylim=c(-0.02,.025),lwd=2,
   xlab="lag",ylab="forecast",cex.axis=1.5,cex.lab=1.5,
   main="IP",cex=1.5)
lines(0:10,ll_IP,type="b",lty=2,lwd=2,pch="o",cex=1.5)
lines(0:10,mean_IP,type="b",lwd=2,pch="*",cex=1.5)
graphics.off()







