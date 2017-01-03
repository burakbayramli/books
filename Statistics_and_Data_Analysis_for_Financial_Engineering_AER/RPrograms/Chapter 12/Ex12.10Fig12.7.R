#  Example 12.10 and Figure 12.7

dat = read.table(file="WeekInt.txt",header=T)
library("car")

attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
fit4 = lm(aaa_dif~cm30_dif)

postscript("weekint_partialresidual.ps",width=6,height=5)  #  Figure 12.7
par(mfrow=c(2,2))
crPlot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
plot(cm10_dif,aaa_dif,main="(c)")
regLine(fit1,col="black",lwd=2,lty="dashed")
plot(cm30_dif,aaa_dif,main="(d)")
regLine(fit4,col="black",lwd=2,lty="dashed")
graphics.off()