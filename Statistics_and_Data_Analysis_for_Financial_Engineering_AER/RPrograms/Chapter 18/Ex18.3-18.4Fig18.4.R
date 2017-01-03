# Example 18.3 and 18.4 and Figure 18.4

library(fGarch)
data(bmw,package="evir")

bmw.garch_norm = garchFit(~arma(1,0)+garch(1,1),data=bmw,cond.dist="norm")
options(digits=3)
summary(bmw.garch_norm)
options(digits=10)


x = bmw.garch_norm@residuals / bmw.garch_norm@sigma.t
n=length(bmw)
grid = (1:n)/(n+1)
fitdistr(x,"t")

postscript("bmwGarch_11_tplot.ps",width=7,height=4)  # Figure 18.4
par(mfrow=c(1,2))
qqnorm(x,datax=T,ylab= "Standardized residual quantiles", 
main="(a) normal plot",
   xlab="normal quantiles")
qqline(x,datax=T)
qqplot(sort(x), qt(grid,df=4),
  main="(b) t plot, df=4",xlab= "Standardized residual quantiles",
  ylab="t-quantiles")
abline(   lm(   qt(c(.25,.75),df=4)~quantile(x,c(.25,.75))   )   )
graphics.off()

bmw.garch_t = garchFit(~arma(1,1)+garch(1,1),cond.dist="std",data=bmw)
options(digits=4)
summary(bmw.garch_t)




#  Example 18.4

bmw.aparch_t = garchFit(~arma(1,0)+aparch(1,1),include.delta=T,
  cond.dist="std",data=bmw)
summary(bmw.aparch_t)

bmw.aparch.std.res.t = bmw.aparch_t@residuals / bmw.aparch_t@sigma.t






