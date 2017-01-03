#  Computes the MLE for the normal-mixture model in Example 5.8
#  15 random starting values are used

library(fGarch)
data(Capm,package="Ecdat")  
diffrf=diff(Capm$rf)
n = length(diffrf)

loglik = function(beta)
{
-sum(  log(  beta[4]* dnorm(diffrf,beta[1],beta[2]) + 
    (1-beta[4])*dnorm(diffrf,beta[1],beta[2]+beta[3])  )   )
}
niter = 15

LB = 0

out = matrix(0,ncol=6,nrow=niter)
for (iter in 1:niter)
{
start=rep(0,4)
start[1] = runif(1,-.01,.01)
start[2] = runif(1,.02,.1)
start[3] = runif(1,.05,.5)
start[4] = runif(1,LB,1)
out[iter,6] = start[4]

fit = optim(start, loglik,method="L-BFGS-B",
   lower=c(-.1,.02,.05,LB),
   upper=c(.1,.2,.3,.99),
   control=list(maxit=1000))

out[iter,1] = 2*fit$value + 2*length(start)
out[iter,2] = 2*fit$value + log(n)*length(start)
out[iter,3:6] = fit$par
}
options(digits=5)
print("  AIC     SBC     beta[1]   beta[2]   beta[3]  beta[4]")
out






