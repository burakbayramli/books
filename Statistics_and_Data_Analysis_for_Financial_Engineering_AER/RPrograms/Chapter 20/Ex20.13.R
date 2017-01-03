#  Example 20.13

library(R2WinBUGS)
library(MASS)  # need to mvrnorm
library(MCMCpack) # need for rwish
library(mnormt)
data(CRSPday,package="Ecdat")
y =CRSPday[,4:7]

#  Fitting symmetric t by profile likelihood

df = seq(5.25,6.75,.01)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){

fit = cov.trob(y,nu=df)
loglik[i] = sum(log(dmt(y,mean=fit$center,S=fit$cov,df=df[i])))
}
options(digits=7)
aic_t = -max(2*loglik)+ 2*(4 + 4*3/2 + 1)

z1 = (2*loglik > 2*max(loglik) - qchisq(.95,1))

plot(df,2*loglik-64000,type="l",cex.axis=1.5,cex.lab=1.5,
   ylab="2*loglikelihood - 64,000",lwd=2)
abline(h = 2*max(loglik) - qchisq(.95,1)-64000)
abline(v=(df[16]+df[17])/2)
abline(v=(df[130]+df[131])/2)

N = dim(y)[1]
m = dim(y)[2]
mu0 = rep(0,m)
Prec_mu = diag(rep(1,m))/10000
Prec_tau =  diag(rep(1,m))/10000
df_wishart =
df_likelihood = 6
df_prior = 3
data=list("y","N","Prec_mu","Prec_tau","mu0","m","df_wishart","df_likelihood",
"df_prior")
inits=function(){list( mu=mvrnorm(1,mu0,diag(rep(1,m)/100) ),
    tau = rwish(4,diag(rep(1,m))/100))}

# Run WinBUGS
multi_t.sim = bugs(data,inits,model.file="mult_t_CRSP.bug",
parameters=c("mu","tau","lambda"),n.chains = 5,n.iter=1200,n.burnin=200,n.thin=1,
bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,debug=F)

print(multi_t.sim,digits=3)

lambdahat = multi_t.sim$mean$lambda
sdinv = diag(1/sqrt(diag(lambdahat)))
cor = sdinv %*% lambdahat %*% sdinv
print(cor,digits=4)

options(digits=4)
cov.trob(y,nu=6,cor=T)  #  MLE of correlation matrix



