library(mnormt)
library(sn)
library(MASS)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
n = dim(dat)[1]

fit = mst.fit(y=dat,plot=F)

aic_skewt = fit$algorithm$objective + 64000 + 2*(4 + 4*3/2 + 1 + 4)

#  Fitting symmetric t by profile likelihood

df = seq(5.25,6.75,length=30)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){
fit = cov.trob(dat,nu=df[i])
loglik[i] = sum(log(dmt(dat,mean=fit$center,S=fit$cov,df=df[i])))
}
options(digits=7)
aic_t = -max(2*loglik)+ 2*(4 + 4 + 4*3/2 + 1) + 64000
bic_t = -max(2*loglik)+ log(n)*(4 + 4 + 4*3/2 + 1) + 64000


z1 = (2*loglik > 2*max(loglik) - qchisq(.95,1))

postscript("CRSPday_MultiT_profile.ps")
plot(df,2*loglik-64000,type="l",cex.axis=1.5,cex.lab=1.5,
   ylab="2*loglikelihood - 64,000",lwd=2)
abline(h = 2*max(loglik) - qchisq(.95,1)-64000)
abline(h = 2*max(loglik)-64000 )
abline(v=(df[2]+df[2])/2)
abline(v=(df[27]+df[28])/2)
graphics.off()

options(digits=4)
cov.trob(dat,nu=6,cor=TRUE)

options(digits=4)
cor(dat)

