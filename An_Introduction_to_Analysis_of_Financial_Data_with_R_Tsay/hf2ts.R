"hf2ts" <- function(da,int,basic=1){
# Compute the two-scale realized volatility for stock returns.
#
# int: time interval in minutes for which returns to be computed.
# basic: in minutes, the time increment to perform subsampling.
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
# Ytot is the tick-by-tick RV.
Ytot=NULL
RV=NULL
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
Ytot=c(Ytot,y1)
#
m1=intraDay(x,basic)
Pr=m1$Price
#
m2=aveRV(Pr,basic,int)
v1=m2$RVbar
RV=c(RV,v1)
#
cnt=cnt+nrow(x)
#print(cnt)
}

nbar = 6.5*60/int
adj=nbar/ntrad
adj1 = 1-adj
correT = Ytot*adj 
X=(RV-correT)/adj1

RV=sqrt(RV)
Ytot=sqrt(Ytot)
X=sqrt(X)


hf2ts <- list(Ytot=Ytot,realized=X, ave.RV=RV,ntrad=ntrad)
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
intv=basic*60
Nob=6.5*3600/intv
# Nob is the number of intervals with length "intv"
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
iend=ist+i*intv
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

"aveRV" <- function(Pr,basic,int){
# computes the average realized volatility for intraday log returns 
# for interval "int"
# The interval "basic" is used as a base to determine the iteration 
# of subsamplings
# The input "Pr" is the transaction price for each interval "basic".
#
multi = int/basic
base = basic*60
Nob = 6.5*3600/base
T = Nob/multi
idx=c(1,c(1:T)*multi+1)
pp = log(Pr[idx])
rtn=diff(pp)
rv=sum(rtn^2)*252

if(multi > 1){
for (j in 2:multi){
jdx=idx+j-1
kdx = jdx[1:T]
pp=log(Pr[kdx])
rtn=diff(pp)
rv = rv+sum(rtn^2)*252
}
}

rv=rv/multi

aveRV <- list(RVbar=rv)
}
