"GMVP" <- function(rtn,start=500){
# compute the weights and variance of global minimum variance portfolio.
# The time period is from (start) to end of the data.
#
# uses cov(x,y) = [var(x+y)-var(x-y)]/4.
#
if(!is.matrix(rtn))rtn=as.matrix(rtn)
#
library(fGarch)
T=dim(rtn)[1]
k=dim(rtn)[2]
wgt = NULL
mVar=NULL
VAR = NULL
ONE=matrix(1,k,1)
prtn=NULL
Det=NULL
for (t in start:T){
# estimate variances and covariances at time "t".
COV=matrix(0,k,k)
for (i in 1:k){
m1=garchFit(~1+garch(1,1),data=rtn[1:t,i],trace=F)
COV[i,i]=volatility(m1)[t]^2
if(i < k){
for (j in (i+1):k){
x=rtn[1:t,i]+rtn[1:t,j]
y=rtn[1:t,i]-rtn[1:t,j]
m2=garchFit(~1+garch(1,1),data=x,trace=F)
m3=garchFit(~1+garch(1,1),data=y,trace=F)
v2=volatility(m2)[t]
v3=volatility(m3)[t]
COV[j,i]=(v2^2-v3^2)/4
COV[i,j]=COV[j,i]
# end of j-loop
}
# end of (if-statement)
}
# end of i-loop
}
Det=c(Det,det(COV))
V=solve(COV)
VAR=rbind(VAR,diag(COV))
Psi=V%*%ONE
W=sum(ONE*Psi)
Psi=Psi/W
wgt=cbind(wgt,Psi)
mVar=c(mVar,1/W)
if(t < T){
prtn=c(prtn,sum(rtn[t+1,]*Psi))
}

}

GMVP <- list(weights=wgt, minVariance=mVar,variances=VAR, returns=prtn,det=Det)
}