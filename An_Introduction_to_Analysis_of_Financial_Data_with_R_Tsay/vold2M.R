"vold2m" <- function(da,ma=0){
# Compute the monthly volatility from daily prices
# ma = 0: Assume no serial correlations in the daily returns
# ma = 1: Assume one-lag of serial correlation in the daily returns
# da: T-by-4 data matrix in the format [Month, Day, Year, Price]
#
if(!is.matrix(da))da=as.matrix(da)
T=nrow(da)
pr=log(da[,4])
rtn=diff(pr)
vol=NULL
pmon=da[1,1]
x=NULL
cnt=NULL
for (t in 2:T){
if(da[t,1]==pmon){
x=c(x,rtn[t-1])
}
else {
v1=length(x)*var(x)
v2=0
if(ma==1)v2=cov1(x)
v1=v1+v2
vol=c(vol,sqrt(v1))
cnt=c(cnt,length(x))
x=c(rtn[t-1])
pmon=da[t,1]
}

}

vold2m <- list(volatility=vol,ndays=cnt)
}

"cov1" <- function(x){
# computes lag-1 covariance matrix
#
xbar=mean(x)
N=length(x)
v2=0
cov1=0
xadj=x-xbar
for (i in 1:(N-1)){
v2=v2+xadj[i]*xadj[i+1]
}
2*v2

cov1=v2*2
}