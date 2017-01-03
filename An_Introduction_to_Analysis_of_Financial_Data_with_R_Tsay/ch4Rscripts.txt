### Chapter 4 ###
da=read.table("m-intcsp7309.txt",header=T)
head(da)
intc=log(da$intc+1)
rtn=ts(intc,frequency=12,start=c(1973,1))
plot(rtn,type='l',xlab='year',ylab='ln-rtn') # time plot
t.test(intc)  # testing the mean of returns
Box.test(intc,lag=12,type='Ljung')
par(mfcol=c(2,1))
acf(intc,lag=24) # ACF plots
acf(abs(intc),lag=24) 
Box.test(abs(intc),lag=12,type='Ljung')
## ARCH test
y=intc-mean(intc)
Box.test(y^2,lag=12,type='Ljung')
source(``archTest.R'')  # R script available on the book web site.
archTest(y,12)   # output edited.
fx=read.table("d-useu9910.txt",header=T)
fxeu=log(fx$rate)
eu=diff(fxeu)
Box.test(eu,lag=20,type='Ljung')
t.test(eu)
Box.test(eu^2,lag=20,type='Ljung')
### source("archTest.R")
archTest(eu,20)
###
library(fGarch) # Load package 
da=read.table("m-intcsp7309.txt",header=T)
head(da)
intc=log(da$intc+1)
m1=garchFit(~1+garch(3,0),data=intc,trace=F) # Fit an ARCH(3) model
summary(m1)
m2=garchFit(~1+garch(1,0),data=intc,trace=F)
summary(m2)
resi=residuals(m2,standardize=T)
tdx=c(1:444)/12+1973
par(mfcol=c(3,10)
plot(tdx,resi,xlab='year',ylab='stand-resi',type='l')
acf(resi,lag=20)
pacf(resi^2,lag=20) 
plot(m2)
#### Student t innovations
m3=garchFit(~1+garch(1,0),data=intc,trace=F,cond.dist="std")
summary(m3)
##
mm1=garchFit(~1+garch(11,0),data=eu,trace=F)
summary(mm1)
##
library(fGarch)
m4=garchFit(~1+garch(1,1),data=intc,trace=F)
summary(m4)
v1=volatility(m4)  # Obtain volatility
resi=residuals(m4,standardize=T) # Standardized residuals
vol=ts(v1,frequency=12,start=c(1973,1))
res=ts(resi,frequency=12,start=c(1973,1))
par(mfcol=c(2,1))  # Show volatility and residuals
plot(vol,xlab='year',ylab='volatility',type='l')
plot(res,xlab='year',ylab='st. resi',type='l') 
par(mfcol=c(2,2)) # Obtain ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24) 
# Obtain plot of predictive intervals
par(mfcol=c(1,1))
upp=0.0113+2*v1
low=0.0113-2*v1
tdx=c(1:444)/12+1973
plot(tdx,intc,xlab='year',ylab='series',type='l',ylim=c(-0.6,0.6))
lines(tdx,upp,lty=2,col='red')
lines(tdx,low,lty=2,col='red')
abline(h=c(0.0113))
# Student-t innovations
m5=garchFit(~1+garch(1,1),data=intc,trace=F,cond.dist="std")
summary(m5)
v2=volatility(m5)
m6=garchFit(~1+garch(1,1),data=intc,trace=F,cond.dist='sstd')
summary(m6)
v3=volatility(m6)
par(mfcol=c(3,1))
plot(tdx,v1,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(a) Gaussian')
plot(tdx,v2,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(b) Student-t')
plot(tdx,v3,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(c) Skew Student-t') 
cor(cbind(v1,v2,v3))
library(fBasics)
basicStats(intc)
tt=-0.5526/sqrt(6/444) # Testing skewness of the data
tt
tt=(0.8717-1)/0.0629 # Testing skewness of the model.
tt
pv=2*pnorm(tt)  # Compute p-value 
pv
plot(m6)
### two-pass analysis
yt=intc-mean(intc)
m1=arima(yt^2,order=c(1,0,1))
m1
mean(intc)
fit=yt^2-m1$residuals
v3=volatility(m6)  # m6 is GARCH(1,1) with skew-t innovations.
cor(v3,sqrt(fit))
####
source("Igarch.R")
mm=Igarch(intc)
names(mm)
###
y=intc*100   # Intel stock returns in percentages
source("garchM.R")  # Compile the script
garchM(y)
sp5=scan(file='sp500.txt')
sp5=sp5*100
m2=garchFit(~1+garch(1,1),data=sp5,trace=F)
summary(m2)
garchM(sp5)
###
source("Egarch.R") # Compile R script
da=read.table("m-ibmsp6709.txt",header=T) # Load data
dim(da)  # Check sample size of the data
ibm=log(da$ibm+1) # Take log transformation
Box.test(ibm,lag=12,type='Ljung') # Check serial correlations
m1=Egarch(ibm) # Model fitting
names(m1)
stresi=m1$residuals/m1$volatility # Obtain standardized residuals
tdx=c(1:516)/12+1967 # Compute time index
par(mfcol=c(2,1)) # Plotting
plot(tdx,ibm,xlab='year',ylab='logrtn',type='l')
plot(tdx,stresi,xlab='year',ylab='stresi',type='l')
Box.test(stresi,lag=10,type='Ljung')  # Model checking
Box.test(stresi,lag=20,type='Ljung')
Box.test(stresi^2,lag=10,type='Ljung')
Box.test(stresi^2,lag=20,type='Ljung')
### TGARCH model
da=read.table(``d-useu9910.txt'',header=T)
fx=log(da$rate)
eu=diff(fx)*100
source('Tgarch11.R')
m1=Tgarch11(eu)
names(m1)
at=m1$residuals
sigt=m1$volatility
resi=at/sigt
Box.test(resi,lag=10,type='Ljung')
Box.test(resi,lag=20,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
Box.test(resi^2,lag=20,type='Ljung')
###
m1=garchFit(~1+aparch(1,1),data=eu,trace=F)
summary(m1)
m2=garchFit(~1+aparch(1,1),data=eu,delta=2,include.delta=F,trace=F)
summary(m2)
plot(m2)
###
da=read.table(``d-useu9910.txt'',header=T)
fx=log(da$rate)
eu=diff(fx)*100
source("Ngarch.R")
m1=Ngarch(eu)
res=m1$residuals
vol=m1$volatility
resi=res/vol
Box.test(resi,lag=10,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
###
da=read.table("d-sp58010.txt",header=T)
x=da[,c(1:3,9)]
dim(x)
source("vold2m.R") ## Compile the script 
m1=vold2m(x)
names(m1)
v1=m1$volatility
cnt=m1$ndays
cnt[1:5]
m2=vold2m(x,ma=1) # Use MA(1) dependence
names(m2)
v2=m2$volatility
da1=read.table("m-sp56710.txt",header=T)
sp=log(da1[,9])
sp5=diff(sp)
library(fGarch)
m3=garchFit(~1+garch(1,1),data=sp5,trace=F)
summary(m3)
v3=volatility(m3)
v3=v3[158:524]
v1=ts(v1,frequency=12,start=c(1980,1))
v2=ts(v2,frequency=12,start=c(1980,1))
v3=ts(v3,frequency=12,start=c(1980,1))
max(v1,v2,v3)
par(mfcol=c(3,1))
plot(v1,xlab='year',ylab='vol',type='l',ylim=c(0,.3))
title(main='(a) No correlations')
plot(v2,xlab='year',ylab='vol',type='l',ylim=c(0,.3))
title(main='(b) Lag-1 correlation')
plot(v3,xlab='year',ylab='vol',type='l',ylim=c(0,.3))
title(main='(c) GARCH(1,1)')
