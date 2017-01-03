library(mnormt)
library(sn)
library(MASS)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]

fit = mst.fit(y=dat,plot=F)

aic_skewt = fit$algorithm$objective + 64000 + 2*(4 + 4*3/2 + 1 + 4)



#  Fitting symmetric t by profile likelihood

df = seq(5.25,6.75,.01)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){
fit = cov.trob(dat,nu=df)
loglik[i] = sum(log(dmt(dat,mean=fit$center,S=fit$cov,df=df[i])))
}
options(digits=7)
aic_t = -max(2*loglik)+ 2*(4 + 4*3/2 + 1) + 64000

z1 = (2*loglik > 2*max(loglik) - qchisq(.95,1))

postscript("CRSPday_MultiT_profile.ps")
plot(df,2*loglik-64000,type="l",cex.axis=1.5,cex.lab=1.5,
   ylab="2*loglikelihood - 64,000",lwd=2)
abline(h = 2*max(loglik) - qchisq(.95,1)-64000)
abline(h = 2*max(loglik)-64000 )
abline(v=(df[16]+df[17])/2)
abline(v=(df[130]+df[131])/2)
graphics.off()

options(digits=4)
cov.trob(dat,nu=6,cor=TRUE)

options(digits=4)
cor(dat)


#####  bootstrapping skewed t fit

n = dim(dat)[1]
nboot = 200
results=matrix(1,nrow=nboot,ncol=4)
t1=proc.time()
for (iboot in 1:nboot)
{
yboot = dat[sample((1:n),n,replace =T),]
fitboot = mst.mle(y=yboot)
results[iboot,] = as.numeric(fitboot$dp$alpha)
}
t2=proc.time()
(t2-t1)/60^2
postscript("CRSPSkewTBootAlpha.ps",width=6,height=5)
par(mfrow=c(2,2))
hist(results[,1],main="GE",xlab=expression(alpha))
hist(results[,2],main="IBM",xlab=expression(alpha))
hist(results[,3],main="Mobil",xlab=expression(alpha))
hist(results[,4],main="CRSP",xlab=expression(alpha))
graphics.off()

for (i in 1:4){ print(quantile(results[,i],c(.025,.975))) }
for (i in 1:4){
print(skewness(results[,i]))
}
for (i in 1:4){
print(kurtosis(results[,i]))
}

postscript("CRSPNormalPlots.ps",width=6,height=5)
par(mfrow=c(2,2))
qqnorm(dat[,1],datax=T,main="GE")
qqline(dat[,1],datax=T)
qqnorm(dat[,2],datax=T,main="IBM")
qqline(dat[,2],datax=T)
qqnorm(dat[,3],datax=T,main="CRSP")
qqline(dat[,3],datax=T)
qqnorm(dat[,4],datax=T,main="CRSP")
qqline(dat[,4],datax=T)
graphics.off()





