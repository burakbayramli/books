#  Example 18.5 and Figure 18.8

library("fGarch")
library("Ecdat")
library("fEcofin")
library("forecast")
data(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)

fit_lm2=lm(diff(log(sp))~diff(gnp.r)+diff(cpi)+diff(bnd),data=new_np)
res_lm2 = rstudent(fit_lm2)
auto.arima(res_lm2)

xregressors = cbind(diff(gnp.r),diff(cpi),diff(bnd))/100
fit_arma = arima(diff(log(sp)),xreg=xregressors,
  order=c(0,0,1))
res_arma = fit_arma$res
par(mfrow=c(1,2))
acf(res_arma)
acf(res_arma^2)

nelploss.garch = garchFit(~arma(0,1)+garch(1,0),data= residuals(fit_lm2))
summary(nelploss.garch)
nelploss.garch.std.res = nelploss.garch@residuals / nelploss.garch@sigma.t
qqnorm(nelploss.garch.std.res,datax=T)

postscript("nelsonPlosser_acf.ps",width=6,height=5)  #  Figure 18.8
par(mfrow=c(2,2))
acf(res_lm2,main="(a) regression: residuals")
acf(res_lm2^2,main="(b) regression: squared residuals")
acf(nelploss.garch.std.res,main="(c) MA/ARCH: residuals")
acf(nelploss.garch.std.res^2,main="(d) MA/ARCH: squared residuals")
graphics.off()



fit_lm3=lm(diff(log(sp))~diff(gnp.r)+diff(cpi)+diff(bnd),data=new_np,
   weights = 1/nelploss.garch@sigma.t^2)
summary(fit_lm3)

plot(fitted(fit_lm2),fitted(fit_lm3))







