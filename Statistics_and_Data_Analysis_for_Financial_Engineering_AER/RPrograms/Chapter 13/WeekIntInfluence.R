dat = read.table(file="WeekInt.txt",header=T)

attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
year2 = year + (month-1)/12 + (day-1)/253
n = length(year2)
year2 = year2[2:n]


fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)

postscript("weeklyInt_regr_diag.ps")
par(mfrow=c(3,2))
plot(year2,aaa_dif,xlab="year",cex.axis=1.5,cex.lab=1.5,main="(a)")
plot(year2,cm10_dif,xlab="year",cex.axis=1.5,cex.lab=1.5,main="(b)")

plot(year2,cm30_dif,xlab="year",cex.axis=1.5,cex.lab=1.5,main="(c)")

plot(year2,hatvalues(fit2),xlab="year",cex.axis=1.5,cex.lab=1.5,
   ylab="leverage",main="(d)")

plot(year2,abs(rstudent(fit2)),xlab="year",cex.axis=1.5,cex.lab=1.5,
   ylab="abs(rstudent)",main="(e)")

plot(year2,sqrt(cooks.distance(fit2)),xlab="year",cex.axis=1.5,cex.lab=1.5,
   ylab="sqrt(Cook's D)",main="(f)")

graphics.off()



