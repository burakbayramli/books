#  Figure 13.5


dat = read.table(file="WeekIntAllData.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
fit = lm(aaa_dif~cm10_dif+cm30_dif)
n=length(cm10)

postscript("WeeklyInterestCookD.ps",width=6,height=5)  #  Figure 13.5
par(mfrow=c(2,2))
plot(hatvalues(fit),ylab="Leverage",xlab="Index",
   main="(a)")
plot(2:n,rstudent(fit),ylab="rstudent",xlab="Index",
 main="(b)")
plot(2:n,cooks.distance(fit),ylab="Cook's D",xlab="Index", main="(c)")
plot(2:n,cooks.distance(fit),ylab="Cook's D",
   xlim=c(368,378),xlab="Index", main="(d)")
graphics.off()
