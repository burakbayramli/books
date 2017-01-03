"Ngarch" <- function(rtn){
# Estimation of a non-symmertic GARCH, NGARCH(1,1), model. 
# Assume normal innovations
# rtn: return series 
#
# The likelihood function "glkn" can be modified to fit more general NGARCH 
#  models.
write(rtn,file='tmp.txt',ncol=1)
# obtain initial estimates
mu=mean(rtn)
par=c(mu,0.01,0.8,0.01,0.7)
#
#
mm=optim(par,glkn,method="Nelder-Mead",hessian=T)
low=c(-10,0,0,0,0)
upp=c(10,1,1,0.4,2)
#mm=optim(par,glkn,method="L-BFGS-B",hessian=T,lower=low,upper=upp)
## Print the results
par=mm$par
H=mm$hessian
Hi = solve(H)
cat(" ","\n")
cat("Estimation results of NGARCH(1,1) model:","\n")
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
sig2t=par[2]+par[3]*ht[i-1]+par[4]*(at[i-1]-par[5]*sqrt(ht[i-1]))^2
ht=c(ht,sig2t)
}
sigma.t=sqrt(ht)
Ngarch <- list(residuals=at,volatility=sigma.t)
}

glkn <- function(par){
rtn=read.table("tmp.txt")[,1]
glkn=0
ht=var(rtn)
T=length(rtn)
if(T > 40)ht=var(rtn[1:40])
at=rtn[1]-par[1]
for (i in 2:T){
ept=rtn[i]-par[1]
at=c(at,ept)
sig2t=par[2]+par[3]*ht[i-1]+par[4]*ht[i-1]*(at[i-1]/sqrt(ht[i-1])-par[5])^2
ht=c(ht,sig2t)
glkn=glkn + 0.5*(log(sig2t) + ept^2/sig2t)
}
glkn
}

