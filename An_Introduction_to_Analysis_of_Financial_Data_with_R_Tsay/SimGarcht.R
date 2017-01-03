"SimGarcht" <- function(h=10,mu=0,alpha=c(0,0.01),b1=0.9,ini=c(0,0),df=5,nter=1000){
# Use simulation to compute risk measures for a GARCH(1,1) model with 
# standardized Student-t innovation.
#
if(df < 2.001)df=5
if(h < 1)h=1
library(fGarch)
nob=h*nter
# generate innovations
ept=matrix(rstd(nob,mean=0,sd=1,nu=df),h,nter)
xt=NULL
for (it in 1:nter){
rt=NULL
at=ini[1]-mu
sigt=ini[2]^2
for(t in 1:h){
sig=alpha[1]+alpha[2]*at[t]^2+b1*sigt[t]
sigt=c(sigt,sig)
at=c(at,sqrt(sig)*ept[t,it])
rt=c(rt,at[t+1]+mu)
}
xt=c(xt,sum(rt))
}
SimGarcht <- list(rtn=xt)
}