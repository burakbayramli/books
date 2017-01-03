"backtestGarch" <- function(rt,orig,h,inc.mean=TRUE,cdist="norm"){
# ar: AR-order, ma: MA-order
# orig: is the starting forecast origin
# rt: the time series
# The GARCH model is GARCH(1,1).
# h: forecast horizon
# 
# inc.mean: flag for constant term of the model (mean-equation).
#
library(fGarch)
T=length(rt)
if(orig > T)orig=T
if(h < 1) h=1
rmse=rep(0,h)
mabso=rep(0,h)
nori=T-orig
err=matrix(0,nori,h)
jlast=T-1
for (n in orig:jlast){
 jcnt=n-orig+1
 x=rt[1:n]
 mm=garchFit(~arma(1,0)+garch(1,1),data=x,include.mean=inc.mean,cond.dist=cdist,trace=F)
 fore=predict(mm,h)
 kk=min(T,(n+h))
# nof is the effective number of forecats at the forecast origin n.
 nof=kk-n
 pred=fore$meanForecast[1:nof]
 obsd=rt[(n+1):kk]
 err[jcnt,1:nof]=obsd-pred
}
#
for (i in 1:h){
iend=nori-i+1
tmp=err[1:iend,i]
mabso[i]=sum(abs(tmp))/iend
rmse[i]=sqrt(sum(tmp^2)/iend)
}
print("RMSE of out-of-sample forecasts")
print(rmse)
print("Mean absolute error of out-of-sample forecasts")
print(mabso)
backtestGarch <- list(origin=orig,error=err,rmse=rmse,mabso=mabso)
}