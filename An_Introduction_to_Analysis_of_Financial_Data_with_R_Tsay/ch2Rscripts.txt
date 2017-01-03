### Chapetr 2 ####
### correlations
da=read.table("m-ibmsp6709.txt",header=T)
head(da)
ibm=da$ibm
sp5=da$sp
cor(sp5,ibm)
cor(sp5,ibm,method='spearman')
cor(sp5,ibm,method='kendall')
# sample ACF
da=read.table("m-dec12910.txt",header=T)
head(da)
d10=da$dec10  # select the Decile 10 returns
dec10=ts(d10,frequency=12,start=c(1967,1))
par(mfcol=c(2,1))
plot(dec10,xlab='year',ylab='returns')
title(main='(a): Simple returns')
acf(d10,lag=24) # command to obtain sample ACF of the data
#
f1=acf(d10,lag=24)
f1$acf
tt=f1$acf[13]*sqrt(516)
tt
# Ljung-Box Q statistics
da=read.table("m-ibmsp6709.txt",header=T)
ibm=da$ibm
lnibm=log(ibm+1) # Transfer to log returns
Box.test(ibm,lag=12,type='Ljung')
Box.test(lnibm,lag=12,type='Ljung')
# AR models
da=read.table("q-gnp4710.txt",header=T)
head(da)
G=da$VALUE
LG=log(G)
gnp=diff(LG)
dim(da)
tdx=c(1:253)/4+1947 # create the time index
par(mfcol=c(2,1))
plot(tdx,G,xlab='year',ylab='GNP',type='l')
plot(tdx[2:253],gnp,type='l',xlab='year',ylab='growth') 
acf(gnp,lag=12)
pacf(gnp,lag=12) # compute PACF
m1=arima(gnp,order=c(3,0,0))
m1
tsdiag(m1,gof=12)  # model checking discussed later
p1=c(1,-m1$coef[1:3]) # set-up the polynomial
r1=polyroot(p1) # solve the polynomial equation
r1
Mod(r1)
k=2*pi/acos(1.616116/1.832674) # compute length of the period
k
#
mm1=ar(gnp,method='mle')
mm1$order # Find the identified order 
names(mm1)
print(mm1$aic,digits=3)
aic=mm1$aic  # For plotting below.
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2)
#
vw=read.table('m-ibm3dx.txt',header=T)[,3]
t1=prod(vw+1)
t1
t1^(12/996)-1
#
vw=read.table('m-ibm3dx2608.txt',header=T)[,3]
m3=arima(vw,order=c(3,0,0))
m3
(1-.1158+.0187+.1042)*mean(vw)  # Compute the intercept phi(0).
sqrt(m3$sigma2) # Compute standard error of residuals
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.35,9) # Compute p value using 9 degrees of freedom
pv
m3=arima(vw,order=c(3,0,0),fixed=c(NA,0,NA,NA)) 
m3
(1-.1136+.1063)*.0089  # compute phi(0)
sqrt(m3$sigma2)  # compute residual standard error
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.83,10)
pv
#
da=read.table("m-ibm3dx2608.txt",header=T)
head(da)
ew=da$ewrtn
m1=arima(ew,order=c(0,0,9)) # unrestricted model
m1
m1=arima(ew,order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))
m1
sqrt(0.005097)
Box.test(m1$residuals,lag=12,type='Ljung')  # model checking
pv=1-pchisq(17.6,9)  # compute p-value after adjusting the d.f.
pv
m1=arima(ew[1:986],order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))
m1
predict(m1,10) # prediction 
#  EACF table
da=read.table("m-3m4608.txt",header=T)
head(da)
mmm=log(da$rtn+1)
library(TSA)     # Load the package 
m1=eacf(mmm,6,12)      # Simplified table 
print(m1$eacf,digits=2)
#  Unit-root test
library(fUnitRoots)
da=read.table("q-gdp4708.txt",header=T)
gdp=log(da[,4])
m1=ar(diff(gdp),method='mle')
m1$order
adfTest(gdp,lags=10,type=c("c"))
#  Unit root
library(fUnitRoots)
da=read.table("d-sp55008.txt",header=T)
sp5=log(da[,7])
m2=ar(diff(sp5),method='mle') # Based on AIC
m2$order
adfTest(sp5,lags=2,type=("ct"))
adfTest(sp5,lags=15,type=("ct")) # Based on PACF
dsp5=diff(sp5)
tdx=c(1:length(dsp5))
m3=arima(dsp5,order=c(2,0,0),xreg=tdx)
m3
m3$coef
sqrt(diag(m3$var.coef))
tratio=m3$coef/sqrt(diag(m3$var.coef))# compute t-ratio
tratio
#
da=read.table(``d-vix0810.txt'',header=T)
vix=log(da$Close)
length(vix)
m1=arima(vix,order=c(0,1,1))
m1
Box.test(m1$residuals,lag=10,type='Ljung')
pp=1-pchisq(14.25,9)
pp
###  seasonal models
da=read.table("q-ko-earns8309.txt",header=T)
head(da)
eps=log(da$value)
koeps=ts(eps,frequency=4,start=c(1983,1))
plot(koeps,type='l')
points(koeps,pch=c1,cex=0.6) 
par(mfcol=c(2,2))
koeps=log(da$value)
deps=diff(koeps)
sdeps=diff(koeps,4)
ddeps=diff(sdeps)
acf(koeps,lag=20)
acf(deps,lag=20)
acf(sdeps,lag=20)
acf(ddeps,lag=20)
# Obtain time plots
c1=c("2","3","4","1")
c2=c("1","2","3","4")
par(mfcol=c(3,1))
plot(deps,xlab='year',ylab='diff',type='l')
points(deps,pch=c1,cex=0.7)
plot(sdeps,xlab='year',ylab='sea-diff',type='l')
points(sdeps,pch=c2,cex=0.7)
plot(ddeps,xlab='year',ylab='dd',type='l')
points(ddeps,pch=c1,cex=0.7) 
#  Estimation
m1=arima(koeps,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
m1
tsdiag(m1,gof=20)  # model checking
Box.test(m1$residuals,lag=12,type='Ljung')
pp=1-pchisq(13.30,10)
pp
koeps=log(da$value)
length(koeps)
y=koeps[1:100]
m1=arima(y,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
m1
pm1=predict(m1,7)
names(pm1)
pred=pm1$pred
se=pm1$se
ko=da$value
fore=exp(pred+se^2/2)
v1=exp(2*pred+se^2)*(exp(se^2)-1)
s1=sqrt(v1)
eps=ko[80:107]
length(eps)
tdx=(c(1:28)+3)/4+2002
upp=c(ko[100],fore+2*s1)
low=c(ko[100],fore-2*s1)
min(low,eps)
max(upp,eps)
plot(tdx,eps,xlab='year',ylab='earnings',type='l',ylim=c(0.35,1.3))
points(tdx[22:28],fore,pch='*')
lines(tdx[21:28],upp,lty=2)
lines(tdx[21:28],low,lty=2)
points(tdx[22:28],ko[101:107],pch='o',cex=0.7)
#
da=read.table("m-deciles08.txt",header=T)
d1=da[,2]
jan=rep(c(1,rep(0,11)),39) # Create January dummy.
m1=lm(d1~jan)
summary(m1)
m2=arima(d1,order=c(1,0,0),seasonal=list(order=c(1,0,1),period=12))
m2
tsdiag(m2,gof=36)  # plot not shown.
m2=arima(d1,order=c(1,0,0),seasonal=list(order=c(1,0,1), period=12),include.mean=F)
m2
# regression models with time series errrors
r1=read.table("w-gs1yr.txt",header=T)[,4]
r3=read.table("w-gs3yr.txt",header=T)[,4]
m1=lm(r3~r1)
summary(m1)
plot(m1$residuals,type='l')
acf(m1$residuals,lag=36)
c1=diff(r1)
c3=diff(r3)
m2=lm(c3~-1+c1)
summary(m2)
acf(m2$residuals,lag=36)
m3=arima(c3,order=c(0,0,1),xreg=c1,include.mean=F)
m3
rsq=(sum(c3^2)-sum(m3$residuals^2))/sum(c3^2)
rsq
#  Long memory
library(fracdiff)
da=read.table("d-ibm3dx7008.txt",header=T)
head(da)
ew=abs(da$vwretd)
# obtain Geweke-Port-Hudak estimate using command fdGPH
m3=fdGPH(ew)
m3
m2=fracdiff(ew,nar=1,nma=1)
summary(m2)
# model comparison
da=read.table("q-gdpc96.txt",header=T)
head(da)
gdp=log(da$gdp)
dgdp=diff(gdp)
m1=ar(dgdp,method='mle')
m1$order
m2=arima(dgdp,order=c(3,0,0))
m2
m3=arima(dgdp,order=c(3,0,0),season=list(order=c(1,0,1),period=4))
m3
source("backtest.R")    # Perform backtest
mm2=backtest(m2,dgdp,215,1)
mm3=backtest(m3,dgdp,215,1)
#
