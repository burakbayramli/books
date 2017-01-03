#  Example 12.11 and Figure 12.8

library("fEcofin")
library("car")
data(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)

fit_lm=lm(diff(log(sp))~diff(gnp.r)+diff(gnp.pc)
   +diff(log(ip))+diff(log(cpi))
   +diff(emp)+diff(bnd),data=new_np)
step_lm = stepAIC(fit_lm)
fit_lm2=lm(step_lm)

postscript("nelsonPlosser_partResid.ps",width=6,height=5) # Figure 12.8

par(mfrow=c(2,2))
crPlot(fit_lm2,var="diff(gnp.r)",main="(a)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit_lm2,var="diff(gnp.pc)",main="(b)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit_lm2,var="diff(log(ip))",main="(c)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit_lm2,var="diff(bnd)",main="(c)",smooth=F,lty=1,lwd=2,col="black")
graphics.off()
