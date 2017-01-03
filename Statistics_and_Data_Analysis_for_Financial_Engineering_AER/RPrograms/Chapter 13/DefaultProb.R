dat = read.table("DefaultData.txt",header=T)
attach(dat)

freq2=freq/100
y = log(freq2[freq2>0])
fit_bow = lm(y~rating[freq>0])
fit_nls = nls(freq2~exp(b1+b2*rating),data=dat,start=list(b1=-5,b2=.5))
fit_tbs = nls(sqrt(freq2)~exp(b1/2+b2*rating/2),
   data=dat,start=list(b1=-6,b2=.5))



sum_nls = summary(fit_nls)
coef_nls = as.numeric(sum_nls$coef[1:2])

sum_tbs = summary(fit_tbs)
coef_tbs = as.numeric(sum_tbs$coef[1:2])

rate_grid = seq(1,16,by=.01)
coef_bow = fit_bow$coefficients

postscript("DefaultProb.ps",width=8,height=4)
par(mfrow=c(1,2))
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_nls[1]+coef_nls[2]*rate_grid))
legend("topleft",c("exponential","data"),lty=c(1,NA),
      pch=c("","*"),pt.cex=c(1, 1.5))
plot(rate_grid, (coef_bow[1]+rate_grid*coef_bow[2]),
      type="l",ylim=c(-14.15,1),xlab="rating",ylab="log(default probability)")
lines(rate_grid,( coef_nls[1]+coef_nls[2]*rate_grid) ,lty=2)
lines(rate_grid,( coef_tbs[1]+coef_tbs[2]*rate_grid) ,lty=6)
points(rating,log(freq2+1e-6))
legend("topleft",c("BOW","nonlinear","tbs","data"),lty=c(1,2,6,NA),
      pch=c("","","","o"))
graphics.off()


postscript("DefaultProbResid1.ps",width=7,height=3.5)

par(mfrow=c(1,2))
fitted_nls = sum_nls$resid+freq2
plot(fitted_nls,abs(sum_nls$resid),xlab="fitted values",
   ylab="absolute residual")
fit_loess  = loess(abs(sum_nls$resid)~ fitted_nls,span=1,deg=1)
ord_nls = order(fitted_nls)
lines(fitted_nls[ord_nls],fit_loess$fit[ord_nls])
qqnorm(sum_nls$resid,datax=T,main="",ylab="sample quantiles",
 xlab="theoretical quantiles")
qqline(sum_nls$resid,datax=T)

graphics.off()


postscript("DefaultProbResid2.ps",width=7,height=3.5)
par(mfrow=c(1,2))
fitted_tbs = sum_tbs$resid+freq2
plot(fitted_tbs,abs(sum_tbs$resid),xlab="fitted values",ylab="absolute residual")
fit_loess_tbs  = loess( abs(sum_tbs$resid)~ fitted_tbs,span=1,deg=1)
ord_tbs = order(fitted_tbs)
lines(fitted_tbs[ord_tbs],fit_loess_tbs$fit[ord_tbs])

qqnorm(sum_tbs$resid,datax=T,main="")
qqline(sum_tbs$resid,datax=T)
graphics.off()





