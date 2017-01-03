#  Figure 5.12

dat=read.csv("FlowData.csv")
dat = dat/10000
library("MASS")
adj = 1.5

postscript("GasFlowsTransProfile.ps",width=6,height=7)
par(mfrow=c(3,3),cex.lab=1.25,cex.axis=1.25)
x=dat$Flow1
x1 = sort(x)
bcfit1 = boxcox(x1~1,lambda=seq(2.6, 4.5, 1/100),
xlab=expression(alpha))
text(3,-1898.75,"Flow 1")
plot(density((x1^3.5-1)/3.5,adjust=adj),main="Flow 1")
qqnorm((x1^3.5-1)/3.5,datax=T,main="Flow 1")
x=dat$Flow2
x2 = sort(x)
bcfit2 = boxcox(x2~1,lambda=seq(8, 13.5, 1/100),xlab=expression(alpha))
text(8.5,-1776.5,"Flow 2")
plot(density( (x2^10.5-1)/10.5,adjust=adj),main="Flow 2")
qqnorm((x2^10.5-1)/10.5,datax=T,main="Flow 2")
x=dat$Flow3
x3 = sort(x)
bcfit3 = boxcox(x3~1,lambda=seq(.6, 2, 1/100),xlab=expression(alpha))
text(1.6,-1793,"Flow 3")
plot(density( (x3^1.235-1)/1.235,adjust=adj),main="Flow 3")
qqnorm((x3^1.235-1)/1.235,datax=T,main="Flow 3")
graphics.off()