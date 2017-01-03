"Egarch" <- function(rtn){
# Estimation of an EGARCH(1,1) model. Assume normal innovations
# rtn: return series 
#
write(rtn,file='tmp.txt',ncol=1)
# obtain initial estimates
mu=mean(rtn)
par=c(mu,0.1,0.1,0.1,0.7)
#
#
#mm=optim(par,glk,method="Nelder-Mead",hessian=T)
low=c(-10,-5,0,-1,0)
upp=c(10,5,1,0,1)
mm=optim(par,glk,method="L-BFGS-B",hessian=T,lower=low,upper=upp)
## Print the results
par=mm$par
H=mm$hessian
Hi = solve(H)
cat(" ","\n")
cat("Estimation results of EGARCH(1,1) model:","\n")
cat("estimates: ",par,"\n")
se=sqrt(diag(Hi))
cat("std.errors: ",se,"\n")
tra=par/se
cat("t-ratio: ",tra,"\n")
# compute the volatility series and residuals
ht=var(rtn)
T=length(rtn)
if(T > 40)ht=var(rtn[1:40])
at=rtn-par[1]
for (i in 2:T){
eptm1=at[i-1]/sqrt(ht[i-1])
lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
sig2t=exp(lnht)
ht=c(ht,sig2t)
}
sigma.t=sqrt(ht)
Egarch <- list(residuals=at,volatility=sigma.t)
}

glk <- function(par){
rtn=read.table("tmp.txt")[,1]
glk=0
ht=var(rtn)
T=length(rtn)
if(T > 40)ht=var(rtn[1:40])
at=rtn[1]-par[1]
for (i in 2:T){
ept=rtn[i]-par[1]
at=c(at,ept)
eptm1=at[i-1]/sqrt(ht[i-1])
lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
sig2t=exp(lnht)
ht=c(ht,sig2t)
glk=glk + 0.5*(lnht + ept^2/sig2t)
}
glk
}

