"foreplot" <- function(pred,rt,orig,start=length(rt),p=0.95){
# pred: is a prediction object, that is, output from "predict" of a
#        fitted ARIMA model.
# orig: is the forecast origin
# rt: the time series
# start: the starting point for ploting.
#
T=length(rt)
if(orig > T)orig=T
fore=pred$pred
#print(fore)
se=pred$se
if (start < 1)start=1
if(start > orig)start=orig
x=c(rt[start:orig],fore)
if(p < 0)p=.95
if(p >= 1)p = .99
pp=1-(1-p)/2
crit=qnorm(pp)
upb=c(rt[start:orig],fore+crit*se)
lowb=c(rt[start:orig],fore-crit*se)
xmax=max(x,upb)
xmin=min(x,lowb)
ran=(xmax-xmin)/10
n=length(x)
tdx=c(1:n)+start-1
plot(tdx,x,type='l',xlab='Time',ylab='values',ylim=c(xmin-ran,xmax+ran))
if(T > orig){
points(c((orig+1):T),rt[(orig+1):T],pch='*')
}
lines(tdx,upb,lty=2)
lines(tdx,lowb,lty=2)
title(main="Forecasting plot")

}