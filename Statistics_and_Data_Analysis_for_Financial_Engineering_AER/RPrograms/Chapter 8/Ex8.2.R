#  Example 8.2 including Figures 8.6 - 8.8 and Table 8.1
#  Using a previous version of R, I was able to fit a Gumbul copula. 
#  With R 2.10.2, there are numerical errors when fitting a Gumbul copula.  This may
#  be due to the poor fit of a Gumbul copula to these data.

library(MASS)
library(fCopulae)
library(copula)
library(sn)
dat=read.csv("FlowData.csv")
dat = dat/10000
x1=dat$Flow1
x1s = sort(x1)

fit1 = st.mle(y=x1)
est1 = fit1$dp
u1 = pst(x1,dp=est1)
n = length(x1)

x2=dat$Flow2
x2s = sort(x2)
fit2 = st.mle(y=x2)
est2 = fit2$dp
u2 = pst(x2,dp=est2)

dem = pempiricalCopula(u1,u2)
ellFit = ellipticalCopulaFit(u1,u2,type="t")
ft = fitCopula(tCopula(-.3,df=20,dim=2),cbind(u1,u2),start=c(-.3,20),method="ml")
fnorm = fitCopula(normalCopula(-.3,dim=2),cbind(u1,u2),start=-.3)
#fgumbel = fitCopula(gumbelCopula(1,dim=2),cbind(u1,u2),start=1,method="ml")  # not working
ffrank = fitCopula(frankCopula(-2,dim=2),cbind(u1,u2),start=-1)
fclayton = fitCopula(claytonCopula(-.2,dim=2),cbind(u1,u2),start=1)

ft  # Results for Table 8.1
fnorm
ffrank
fclayton

postscript("unif_flows_hist_plot.ps",width=6,height=6)  #  Figure 8.6
par(mfrow=c(2,2),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
hist(u1,main="(a)",xlab=expression(u[1]))
hist(u2,main="(b)",xlab=expression(u[2]))
plot(u1,u2,main="(c)",xlab=expression(u[1]),ylab=expression(u[2]))
graphics.off()

postscript("norm_flows_hist_plot.ps",width=6,height=6)  # Figure 8.7
z1 = qnorm(u1)
z2= qnorm(u2)
par(mfrow=c(2,2),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
qqnorm(z1,datax=T,main="Normal QQ Plot")
qqnorm(z2,datax=T,main="Normal QQ Plot")
plot(z1,z2,xlab=expression(z[1]),ylab=expression(z[2]))
graphics.off()

postscript("unif_flows_contours_copulas.ps",width=6,height=5)  # Figure8.8
par(mfrow=c(2,3))
plot(u1,u2,main="Data",xlab=expression(u[1]),ylab=expression(u[2]))
contour(dem$x,dem$y,dem$z,xlab="Flow 1",main="Empirical")
contour(normalCopula(-.33),pcopula,main="Normal")
contour(gumbelCopula(1.23,dim=2),pcopula,main="Gumbel")
contour(frankCopula(-2.25,dim=2),pcopula,main="Frank")
contour(claytonCopula(-.166,dim=2),pcopula,main="Clayton")
graphics.off()










