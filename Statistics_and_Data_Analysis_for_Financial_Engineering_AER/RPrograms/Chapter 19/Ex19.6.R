#  Example 19.6

library(mnormt)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:6]

#  Fitting symmetric t by profile likelihood

df = seq(5.25,6.75,.01)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){

fit = cov.trob(dat,nu=df)
loglik[i] = sum(log(dmt(dat,mean=fit$center,S=fit$cov,df=df[i])))
}

indicate = (1:length(df))[ (loglik== max(loglik)) ]
dfhat = df[indicate]
estim = cov.trob(dat,nu=dfhat,cor=TRUE)
muhat = estim$center
covhat = estim$cov
w = rep(1/3,3)
muP = as.numeric(w %*% muhat)
varP = as.numeric(w %*% covhat %*% w)
sdP = sqrt(varP)
lambdaP = sqrt((dfhat-2)/dfhat) * sdP
alpha = .05
VaR = -20000*(muP + lambdaP* qt(alpha,dfhat))
qalpha = qt(alpha,df=dfhat)
es1 = dt(qalpha,df=dfhat)/(alpha)
es2 = (dfhat + qalpha^2) / (dfhat - 1)
es3 = -muP+lambdaP*es1*es2
ES = 20000*es3

muhat
covhat
muP
sdP
lambdaP
VaR
ES







