#  Example 13.8 and Figure 13.11

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )

fit1 = lm(aaa_dif~cm10_dif+cm30_dif)
fit2 = lm(aaa~cm10+cm30)

summary(fit1)
summary(fit2)

postscript("WeekIntNoDifResid.ps",height=3.5,width=7)     #  Figure 13.11
par(mfrow=c(1,2),cex.axis=1.05,cex.lab=1.05,lwd=1,cex.main=1.05)
plot(residuals(fit2),ylab="Residuals",main="(a)")
acf(residuals(fit2),main="(b) Residuals")
graphics.off()





