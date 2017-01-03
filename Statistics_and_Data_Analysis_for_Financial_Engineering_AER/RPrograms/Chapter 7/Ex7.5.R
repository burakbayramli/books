# Ex 7.5
#   The results on R 2.12.0 differ somewhat from those in the book
#   Moreover, if the maximum number of function evaluation and iterations
#   are increased beyond the default values, then the estimates change
#   considerably, at least for the alphas.  Compare fit1 and fit2 below

library(mnormt)
library(sn)
library(MASS)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]

fit1 = mst.fit(y=dat,plot=F)
fit2 = mst.fit(y=dat,plot=F, control=list(eval.max=1000,iter.max=500))
fit1$dp
fit2$dp 
aic_skewt = fit2$algorithm$objective + 64000 + 2*(4 +4 + 4*3/2 + 1 + 4)

aic_skewt

#  The following was not used in the book, but shows that there
#  is not a lot of skewness in these returns
par(mfrow=c(2,2))
for (i in 1:4){
qqnorm(dat[,i],datax=T)
qqline(dat[,i],datax=T)}


