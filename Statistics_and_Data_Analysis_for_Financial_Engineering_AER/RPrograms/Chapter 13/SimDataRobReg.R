#  SimDataRobReg.R

library(robust)
library("faraway")
set.seed(99)
x = 1:11
x[11] = 50
y=1+x+rnorm(11)
y2 = y
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19)

postscript("leverage_ex.ps",width=6,height=5)
par(mfrow=c(2,2),lwd=1)
plot(x,y,ylim=c(0,60),cex=c(rep(1.25,10),1.5),pch=cexx,main="(a)")
abline(lm(y~x),lwd=2)
plot(x,y2,ylim=c(0,60),cex=c(rep(1.25,10),1.5),ylab="y",pch=cexx,main="(b)")
abline(lm(y2~x),lwd=2)
plot(x2,y,ylim=c(0,60),cex=c(rep(1.15,10),1.5),xlab="x",pch=cexx,main="(C)")
abline(lm(y~x2),lwd=2)
graphics.off()

postscript("leverage_ex_lev.ps",width=6,height=5)
par(mfrow=c(2,2),lwd=1,pch=19)
plot(hatvalues(lm(y~x)),ylab="leverage",main="(a)",ylim=c(0,1))
plot(hatvalues(lm(y2~x)),ylab="leverage",main="(b)",ylim=c(0,1))
plot(hatvalues(lm(y~x2)),ylab="leverage",main="(c)",ylim=c(0,1))
plot(x2,hatvalues(lm(y~x2)),xlab="x",ylab="leverage",
    main="(d)",ylim=c(0,1))
graphics.off()

postscript("leverage_ex_cookD.ps",width=6,height=5)
par(mfrow=c(2,3),cex.axis=1,cex.lab=1,lwd=1,pch=19)
plot(sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",ylim=c(0,11))
halfnorm( sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",
  xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",
  xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",
  xlim=c(0,1.85))
graphics.off()


postscript("leverage_ex_residuals.ps",width=6,height=5)
par(mfrow=c(2,2),cex.axis=1.15,lwd=1,pch=19)
plot(residuals(lm(y~x)),ylab="residual",main="(a)")

plot(residuals(lm(y2~x)),ylab="residual",main="(b)")
plot(residuals(lm(y~x2)),ylab="residual",main="(c)")
graphics.off()

postscript("leverage_ex_rstudent.ps",width=6,height=6)
par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15,lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="(a)")

plot(rstudent(lm(y2~x)),ylab="studentized residual",main="(b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="(c)")
graphics.off()


fit_r =lmrob(y2~x)
fit_lts =ltsReg(y2~x)
fit_lms = lmsreg(y2~x)
fit_ls = lm(y2~x)


par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.5,pch="*")
plot(x,y2,cex=15,pch=".",cex.axis=1.15,cex.lab=1.15,ylab="y")
lines(x,fit_r$fitted,lwd=2)
lines(x,fit_ls$fitted,lty=3,lwd=3)
lines(x,fit_lms$fitted,lty=4,lwd=2,col="red")
legend(20,4,c("data","LS fit","MM fit"),pch=c(".",NA,NA),pt.cex=c(15,1,1),
  lty=c(0,3,1),lwd=c(1,3,2),cex=1.5)



postscript("SimDataRobReg.ps",width=6,height=5)
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


################
postscript("leverage_ex_residualsNEW.ps",width=6,height=5)
par(mfrow=c(2,3),lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="Dataset (a)")
plot(rstudent(lm(y2~x)),ylab="studentized residual",main="Dataset (b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="Dataset (c)")
plot(residuals(lm(y~x)),ylab="residual",main="Dataset (a)")
plot(residuals(lm(y2~x)),ylab="residual",main="Dataset (b)")
plot(residuals(lm(y~x2)),ylab="residual",main="Dataset (c)")
graphics.off()



