"RMeasure.sim" <- function(mu,garchpar,vol,h=10,cond.dist="std",df=0,skew=0,iter=50000){
## Use simulation to compute the VaR and ES of a 
# GARCH(1,1) model with Student-t innovations
# input: mu: mean of the return
#        garchpar = (omega,alpha,beta)
#        vol: 1-step ahead volatility prediction at the forecast origin
#        df: degrees of freedom
#        iter: number of iterations
#
## It requires "fGarch" package
##
#
nob=h*iter
if(cond.dist=="std"){
epsilon=rstd(nob,mean=0,sd=1,nu=df)
}
else{
epsilon=rsstd(nob,mean=0,sd=1,nu=df,xi=skew)
}
Ep=matrix(epsilon,iter,h)
##
At=matrix(Ep[,1]*vol,iter,1)
Sig2=matrix(vol^2,iter,1)
Rtn=At+mu
for (ii in 2:h){
V2=garchpar[1]+garchpar[2]*At[,(ii-1)]^2+garchpar[3]*Sig2[,(ii-1)]
At=cbind(At,Ep[,ii]*sqrt(V2))
Rtn=cbind(Rtn,At[,ii]+mu)
Sig2=cbind(Sig2,V2)
}
prob=c(0.95,0.99,0.999)
rtn=apply(Rtn,1,sum)
VaR=quantile(rtn,prob)
ES=NULL
ll=length(prob)
for (i in 1:ll){
idx=c(1:iter)[rtn < VaR[i]]
ES=c(ES,mean(rtn[-idx]))
}
tbl=cbind(prob,VaR,ES)
colnames(tbl) <- c("prob.","VaR","ES")
cat("Riskmeasures: ","\n")
print(tbl)

RMeasure.sim <- list(rtn=rtn)
}