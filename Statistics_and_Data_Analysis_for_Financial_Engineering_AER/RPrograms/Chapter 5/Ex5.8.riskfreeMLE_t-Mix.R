#  Computes the MLE for the t-mixture model in Example 5.8
#  15 random starting values are used
#  This model seems overparameterized (see text)

library(fGarch)
data(Capm,package="Ecdat")  
diffrf=diff(Capm$rf)
n = length(diffrf)

loglik = function(beta)
{
sum(   - log(  beta[5]*dstd(diffrf,beta[1],beta[2],beta[4]) + 
    (1-beta[5])*dstd(diffrf,beta[1],beta[2]+beta[3],beta[4])  )   )
}
niter = 15

out = matrix(0,ncol=8,nrow=niter)
for (iter in 1:niter)
{
start=rep(0,5)
start[1] = runif(1,-.01,.01)
start[2] = runif(1,.001,.05)
start[3] = runif(1,.001,.5)
start[4] = runif(1,2.1,60)
start[5] = runif(1,0 ,1)
out[iter,8] = start[4]

LB = 0
fit = optim(start, loglik,method="L-BFGS-B",
   lower=c(-Inf,.0001,.0001,2.1,LB),
   upper=c(Inf,Inf,Inf,Inf,1),
   control=list(maxit=1000))

out[iter,1] = 2*fit$value + 10
out[iter,2] = 2*fit$value + log(n)*5
out[iter,3:7] = fit$par
}
options(digits=5)
print(" AIC    BIC  beta[1] beta[2] beta[3] beta[4] beta[5] start_for_beta[4]")
out









