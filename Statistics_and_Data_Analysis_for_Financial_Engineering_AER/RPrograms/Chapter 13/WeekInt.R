dat = read.table(file="WeekInt.txt",header=T)
library("car")

attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )

postscript("cm10aaa.ps",width=6,height=5)
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate",
   ylab="change in AAA rate")
graphics.off()

postscript("weekint_splotm.ps",width=6,height=5)
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))
graphics.off()

fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
fit4 = lm(aaa_dif~cm30_dif)

summary(lm(aaa_dif~ff_dif))
cor(aaa_dif,ff_dif)

options(digits=3)
anova(lm(aaa_dif~ff_dif+cm30_dif+cm10_dif))

anova(lm(aaa_dif~cm10_dif+cm30_dif+ff_dif))

anova(lm(aaa_dif~cm30_dif+cm10_dif+ff_dif))





postscript("weekint_partialresidual.ps",width=6,height=5)
par(mfrow=c(2,2))
cr.plot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
cr.plot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
plot(cm10_dif,aaa_dif,main="(c)")
reg.line(fit1,col="black",lwd=2,lty="dashed")
plot(cm30_dif,aaa_dif,main="(d)")
reg.line(fit4,col="black",lwd=2,lty="dashed")
graphics.off()



# experimenting with av plots.
par(mfrow=c(3,2))
cr.plot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
cr.plot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
av.plot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
av.plot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
plot(cm10_dif,aaa_dif,main="(c)")
reg.line(fit1,col="black",lwd=2,lty="dashed")
plot(cm30_dif,aaa_dif,main="(d)")
reg.line(fit4,col="black",lwd=2,lty="dashed")


library(faraway)
vif(fit3)

options(digits=2)
summary(fit1)
summary(fit2)
summary(fit3)


library(leaps)

subsets = regsubsets(aaa_dif~.,
  data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1)
b=summary(subsets)

postscript("WeekInt_model_selection.ps",width=6,height=3)
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables",
   ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",
   ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables",
   ylab="adjusted R2")

graphics.off()











