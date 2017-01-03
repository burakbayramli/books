library("fEcofin")

data(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)

postscript("nelsonPlosser_differences.ps",width=6,height=5)  #  Figure 12.6
par(mfrow=c(2,3),cex.lab=1.35)
plot(year,diff(gnp.r),type="b",ylab="differences",main="gnp.r")
plot(year,diff(log(gnp.r)),type="b",ylab="differences",main="log(gnp.r)")
plot(year,diff(sqrt(gnp.r)),type="b",ylab="differences",main="sqrt(gnp.r)")r
plot(year,diff(ip),type="b",ylab="differences",main="ip")
plot(year,diff(log(ip)),type="b",ylab="differences",main="log(ip)")
plot(year,diff(sqrt(ip)),type="b",ylab="differences",main="sqrt(ip)")
graphics.off()

fit_lm=lm(diff(log(sp))~diff(gnp.r)+diff(gnp.pc)
   +diff(log(ip))+diff(log(cpi))
   +diff(emp)+diff(bnd),data=new_np)
summary(fit_lm)                #  Example 12.9

print(vif(fit_lm),digits=2)

options(digits=4)
step_lm = stepAIC(fit_lm)

x1= as.data.frame(cbind(diff(gnp.r),diff(gnp.pc),diff(log(ip)),
   diff(log(cpi)),
   diff(emp),diff(bnd)))
names_x1 = c("gnp.r", "gnp.pc","log(ip)","log(cpi)",
   "emp","bnd")

leaps.fit = leaps(y =diff(log(sp)),names=names_x1,x=x1,nbest=1)
options(digits=2)
cbind(leaps.fit$which,leaps.fit$Cp)


