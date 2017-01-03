library("tseries")
library("fGarch")
library("Ecdat")
library("fEcofin")
library("faraway")
library("leaps")
library("car")

data(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)

adf.test(diff(log(sp)))
adf.test(diff(gnp.r))
adf.test(diff(gnp.pc))
adf.test(diff(log(ip)))
adf.test(diff(emp))
adf.test(diff(bnd))
kpss.test(diff(bnd))

plot(diff(bnd))


fit_lm=lm(diff(log(sp))~diff(gnp.r)+diff(gnp.pc)
   +diff(log(ip))+diff(log(cpi))
   +diff(emp)+diff(bnd),data=new_np)
summary(fit_lm)
print(vif(fit_lm),digits=2)
cor(diff(gnp.r),diff(gnp.pc))
step_lm = stepAIC(fit_lm)
summary(lm(step_lm))
x1= as.data.frame(cbind(diff(gnp.r),diff(gnp.pc),diff(log(ip)),
   diff(log(cpi)),
   diff(emp),diff(bnd)))
names_x1 = c("gnp.r", "gnp.pc","log(ip)","log(cpi)",
   "emp","bnd")

par(mfrow=c(3,3))
plot(diff(log(sp)),type="b")
for (i in 1:6)
{
plot(x1[,i],type="b")
}

par(mfrow=c(3,3))
qqnorm(diff(log(sp)))
for (i in 1:6)
{
qqnorm(x1[,i])
}


leaps.fit = leaps(y =diff(log(sp)),names=names_x1,x=x1,nbest=1)
options(digits=2)
cbind(leaps.fit$which,leaps.fit$Cp)

fit_lm2=lm(step_lm)
summary(fit_lm2)
res_lm2 = fit_lm2$res

postscript("nelsonPlosser_partResid.ps",width=6,height=5)
par(mfrow=c(2,2))
cr.plot(fit_lm2,var="diff(gnp.r)",main="(a)",smooth=F,lty=1,lwd=2,col="black")
cr.plot(fit_lm2,var="diff(gnp.pc)",main="(b)",smooth=F,lty=1,lwd=2,col="black")
cr.plot(fit_lm2,var="diff(log(ip))",main="(c)",smooth=F,lty=1,lwd=2,col="black")
cr.plot(fit_lm2,var="diff(bnd)",main="(c)",smooth=F,lty=1,lwd=2,col="black")
graphics.off()

plot(diff(gnp.pc),diff(log(sp)))

summary(lm(diff(log(sp))~diff(gnp.pc)+
   +diff(log(ip))+diff(bnd),data=new_np))



