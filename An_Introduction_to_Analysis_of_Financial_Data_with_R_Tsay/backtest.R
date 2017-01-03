"backtest" <- function(m1,rt,orig,h,xre=NULL,fixed=NULL,inc.mean=TRUE){
# m1: is a time-series model object
# orig: is the starting forecast origin
# rt: the time series
# xre: the independent variables
# h: forecast horizon
# fixed: parameter constriant
# inc.mean: flag for constant term of the model.
#
regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),period=m1$arma[5])
T=length(rt)
if(!is.null(xre) && !is.matrix(xre))xre=as.matrix(xre)
ncx=ncol(xre)
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
 if (!is.null(xre)){
 pretor=xre[1:n,]
 mm=arima(x,order=regor,seasonal=seaor,xreg=pretor,fixed=fixed,include.mean=inc.mean)
 nx=xre[(n+1):(n+h),]
 if(h==1)nx=matrix(nx,1,ncx)
 fore=predict(mm,h,newxreg=nx)
}
else {
  mm=arima(x,order=regor,seasonal=seaor,xreg=NULL,fixed=fixed,include.mean=inc.mean)
  fore=predict(mm,h,newxreg=NULL)
}
 kk=min(T,(n+h))
# nof is the effective number of forecats at the forecast origin n.
 nof=kk-n
 pred=fore$pred[1:nof]
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
backtest <- list(origin=orig,error=err,rmse=rmse,mabso=mabso)
}