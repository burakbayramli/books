# Figure 14.17

library(robust)
set.seed(99)
x = 1:11
x[11] = 50
y=1+x+rnorm(11)
y2 = y
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19)

fit_r =lmrob(y2~x)
fit_lts =ltsReg(y2~x)
fit_lms = lmsreg(y2~x)
fit_ls = lm(y2~x)

postscript("SimDataRobReg.ps",width=6,height=5)  #  Figure 14.17
par(mfrow=c(2,2),pch="*")
plot(x,y,ylim=c(0,60),cex=3,main="(a)")
abline(ltsReg(y~x),lwd=2)
abline(lm(y~x),lty=2,lwd=2)
plot(x,y2,ylim=c(0,60),cex=3,ylab="y",main="(b)")
abline(ltsReg(y2~x),lwd=2)
abline(lm(y2~x),lty=2,lwd=2)
plot(x2,y,ylim=c(0,60),cex=3,xlab="x",main="(c)")
abline(ltsReg(y~x2),lwd=2)
abline(lm(y~x2),lwd=2,lty=2)
graphics.off()