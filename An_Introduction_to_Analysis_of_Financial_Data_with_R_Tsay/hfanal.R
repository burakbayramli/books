"hfanal" <- function(da,int,basic=1){
# Compute intraday LOG returns & realized volatility & number of trades in 
# each trading day.
#
# int: time interval in minutes for which returns to be computed.
# basic: in minutes, the base interval to process transaction data.
# The idea of subsampling is to be done shortly. [September 14, 2010.]
#
# da: data in the format: date, hour, minute, second, price, volume
#
if(!is.matrix(da))da=as.matrix(da)
# First, remove trade outside of the normal trading hours
istart=9*60*60+30*60
iend=16*60*60
timemid=da[,2]*3600+da[,3]*60+da[,4]
da=cbind(da[,1],timemid,da[,5])
colnames(da) <- c("Date","Time","Price")
T0=nrow(da)
idx=c(1:T0)[da[,2] >= istart]
da=da[idx,]
T1=nrow(da)
jdx=c(1:T1)[da[,2] <= iend]
da=da[jdx,]
T=nrow(da)
##
print(c(T0,T1,T))

Ytot=NULL
RV=NULL
logrtn=NULL
ntrad=NULL
cnt = 0
# Process through days
while (cnt < T){
date=da[(cnt+1),1]
idx=c(1:T)[da[,1]==date]
x=da[idx,]
ntrad=c(ntrad,nrow(x))
pp=log(x[,3])
r1=diff(pp)
y1=sum(r1^2)*252
Ytot=c(Ytot,sqrt(y1))
#
m1=intraDay(x,basic)
Pr=m1$Price
m2=intraRtn(Pr,basic,int)
rtn=m2$returns
v1=sum(rtn^2)*252
RV=c(RV,sqrt(v1))
#
logrtn=c(logrtn,rtn)
cnt=cnt+nrow(x)
print(cnt)
}

hfanal <- list(returns=logrtn,Ytot=Ytot,realized=RV,ntrad=ntrad)
}

"intraDay" <- function(da,basic){
# da: matrix consisting of trade-by-trade
# The format is Date, Time and Price (Time is in seconds from midnight)
# basic: base time interval (measured in minutes)
#
# The program basically creates a series of intraday price series for the 
# base interval "basic".
# 
#
if(!is.matrix(da))da=as.matrix(da)
ist = 9*3600+30*60
int=basic*60
Nob=6.5*3600/int
# Nob is the number of intervals with length "int"
y=NULL
T=nrow(da)
idx=c(1:T)[da[,2]==ist]
if(length(idx)<1){
y=da[1,3]
jused = 1
}
else {
y=da[length(idx),3]
jused = length(idx)
}
for (i in 1:Nob){
iend=ist+i*int
idx=c(1:T)[(da[,2]-iend) <= 0]
jj=idx[length(idx)]
if(jj < jused){
y=c(y,y[length(y)])
}
else {
jused=jj
y=c(y,da[jj,3])
}
}
intraDay <- list(Price=y)
}

"intraRtn" <- function(Pr,basic,int){
# computes the intradaily returns of interal "int".
# The input "Pr" is the transaction price for each interval "basic".
#
multi = int/basic
base = basic*60
intval=int*60
Nob = 6.5*3600/base
T = Nob/multi
idx=c(1,c(1:T)*multi+1)
pp = log(Pr[idx])
rtn=diff(pp)

intraRtn <- list(returns=rtn)
}
