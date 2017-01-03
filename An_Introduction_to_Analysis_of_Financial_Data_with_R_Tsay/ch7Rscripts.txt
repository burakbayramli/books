### Chapter 7 ####
### RiskMetrics
da=read.table("d-ibm-0110.txt",header=T)
head(da)
ibm=log(da[,2]+1)*100
source("RMfit.R")
mm=RMfit(ibm)
##
da1=read.table("d-useu0111.txt",header=T)
head(da1)
par(mfcol=c(2,1))
rate=da1$rate; plot(rate,type='l')
rt=diff(log(rate)); plot(rt,type='l')
m2=RMfit(rt)
###
da=read.table("d-ibm-0110.txt",header=T)
xt=-log(da$return+1) # calculate negative log returns.
library(fGarch)
m1=garchFit(~garch(1,1),data=xt,trace=F)
m1
predict(m1,3)
source("RMeasure.R")
m11=RMeasure(-.000601,.0078243)
m2=garchFit(~garch(1,1),data=xt,trace=F,cond.dist="std")
m2
predict(m2,3)
m22=RMeasure(-.0004113,.0081009,cond.dist="std",df=5.751)
### multi-step
M1=predict(m1,15) # Model m1 is defined in the output of Example 7.5.
names(M1)
mf=M1$meanForecast
merr=M1$meanError
pmean=sum(mf)
pvar=sum(merr^2)
pstd=sqrt(pvar)
pmean
pvar
pstd
M11=RMeasure(pmean,pstd)
###
source("SimGarcht.R")
vol=volatility(m2)
a1=c(1.922*10^(-6),0.06448); b1=0.9286; mu=-4.113*10^(-4)
ini=c(ibm[2515],vol[2515])
mm=SimGarcht(h=15,mu=mu,alpha=a1,b1=b1,df=5.751,ini=ini,nter=30000)
rr=mm$rtn
mean(rr)
quantile(rr,c(0.95,0.99))  # Obtain VaR
idx=c(1:30000)[rr 0.04797729] # Compute ES for p = 0.05
mean(rr[idx])
idx=c(1:30000)[rr 0.07839338] # Compute ES for p = 0.01
mean(rr[idx])
### empirical quantiles
da=read.table("d-ibm-0110.txt",header=T)
ibm=-log(da[,2]+1)
prob1=c(0.9,0.95,0.99,0.999) # probabilities of interest
quantile(ibm,prob1)
sibm=sort(ibm) # Sorting into increasing order
0.95*2515
es=sum(sibm[2390:2515])/(2515-2389)
es
#### Quantitle regression 
dd=read.table("d-ibm-rq.txt",header=T) # Load data
head(dd)
dim(dd)
dd[,3]=dd[,3]/100
library(quantreg)
mm=rq(nibm~vol+vix,tau=0.95,data=dd) # Quantile regression
summary(mm)
names(mm)
fit=mm$fitted.values
tdx=c(2:2515)/252+2001
plot(tdx,dd$nibm,type='l',xlab='year',ylab='neg-log-rtn')
lines(tdx,fit,col='red')
v1[2515]
vix[2515]
vfit=-.00104+1.17724*v1[2515]+0.02809*vix[2515]/100
vfit
mm=rq(xt~vol+vix,tau=0.99,data=dd) # 99th quantile
summary(mm)
### EVT
da=read.table("d-ibm-0110.txt",header=T)
ibm=log(da$return+1)*100
xt=-ibm
source("Hill.R") # compile R script
Hill
Hill(ibm,110)
Hill(xt,110)
library(evir) # Load package
par(mfcol=c(2,1))
hill(ibm,option=c("xi"),end=200)
hill(xt,option=c("xi"),end=200)
help(hill)
m1=gev(xt,block=21)
m1
plot(m1)
### return level
da=read.table("d-ibm-0110.txt",header=T)
xt=-log(da[,2]+1)*100
library(evir)
m1=gev(xt,block=21)   # GEV estimation with sub-period length 21.
rl.21.12=rlevel.gev(m1,k.block=12)
rl.21.12               # Output plot is not shown.
###
da=read.table("d-ibm-0110.txt",header=T)
ibm=log(da[,2]+1)
library(evir)
par(mfcol=c(2,1))
xt=-ibm
qplot(xt,threshold=0.01,pch='*',cex=0.8,main="Loss variable of daily IBM log returns")
meplot(ibm)
title(main="Daily IBM log returns")
### POT
da=read.table("d-ibm-0110.txt",header=T)
ibm=log(da[,2]+1)
xt=-ibm
m1=pot(xt,threshold=0.01)
m1
plot(m1)
riskmeasures(m1,c(0.95,0.99))
riskmeasures(m2,c(0.95,0.99)) # Threshold=0.012
riskmeasures(m3,c(0.95,0.99)) # Threshold=0.008
#### GPD 
library(evir) 
da=read.table("d-ibm-0110.txt",header=T)
ibm=log(da[,2]+1)
xt=-ibm
m1gpd=gpd(xt,threshold=0.01)
m1gpd
names(m1gpd)
par(mfcol=c(2,2))
plot(m1gpd)
riskmeasures(m1gpd,c(0.95,0.99))
###
library(evir)
help(exindex)
m1=exindex(xt,10) # Estimate the extremal index of Figure 7.10.
      # VaR calculation.
v1=1.966-(1.029/.251)*(1-(-21*.72*log(.99))^(-.251))
v1
