### Chapter 6 ###
da=read.table("taq-cat-t-jan042010.txt",header=T)
head(da)
vol=da$size/100
da1=read.table("taq-cat-cpch-jan042010.txt")
cpch=da1[,1]   # category of price change
pch=da1[,2]  # price change
cf=as.factor(cpch)  # create categories in R
length(cf)
y=cf[4:37715]
y1=cf[3:37714]  # create indicator variables for lag-1 cpch
y2=cf[2:37713]  # create indicator variables for lag-2 cpch
vol=vol[2:37716]
v2=vol[2:37713] # create lag-2 volume
cp1=pch[3:37714] # select lagged price changes
cp2=pch[2:37713]; cp3=pch[1:37712]
library(MASS) # load package
m1=polr(y~v2+cp1+cp2+cp3+y1+y2,method="probit")
summary(m1)
names(m1)
yhat=m1$fitted.values
print(yhat[1:5,],digits=3)
####  ADS models
da=read.table("taq-cat-cpch-jan042010.txt")
dim(da)
pch=da[,2]  # create Ai, Di, and Si and their lagged variables
idx=c(1:37715)[pch > 0]
jdx=c(1:37715)[pch < 0]
A=rep(0,37715); A[idx]=1; A[jdx]=1
D=rep(0,37715); D[idx]=1; D[jdx]=-1
S=abs(da[,1]-4)
Ai=A[2:37715]; Aim1=A[1:37714]
Di=D[2:37715]; Dim1=D[1:37714]
Si=S[2:37715]; Sim1=S[1:37714]
m1=glm(Ai~Aim1,family="binomial")
summary(m1)
di=Di[Ai==1]
dim1=Dim1[Ai==1]
di=(di+abs(di))/2 # transform di to binary
m2=glm(di~dim1,family="binomial")
summary(m2)
si=Si[Di==1]
sim1=Sim1[Di==1]
source("GeoSize.R") # R script to fit Geometric dist.
m3=GeoSize(si,sim1)
nsi=Si[Di==-1]
nsim1=Sim1[Di==-1]
m4=GeoSize(nsi,nsim1)
#####
da=read.table("taq-cat-t-jan04t082010.txt",header=T)
head(da)
sec=3600*da$hour+60*da$minute+da$second # time in seconds
ist=3600*9+30*60;  end=3600*16
lunch=3600*12
length(sec)
idx=c(1:155267)[sec < ist]   # before market opens
jdx=c(1:155267)[sec > end]  # after market closes
sec=sec[-c(idx,jdx)]  # normal trading hours only.
length(sec)
dt=diff(sec)
kdx=c(1:length(dt))[dt > 0] # Positive durations only
length(kdx)
ti=sec[2:155077]
dt=dt[kdx]
ti=ti[kdx]
plot(dt,type='l',xlab='index',ylab='duration')
st=3600*6.5
f1=(ti-lunch)/st
ft=cbind(f1,f1^2)
m2=lm(log(dt)~ft)  # Linear model for log(durations)
summary(m2)
names(m2)
fit=m2$fitted.values
adjdt=dt/exp(fit)
##
source("acd.R")
m2=acd(adjdt,order=c(1,1),cond.dist="exp")
names(m2)
m3=acd(adjdt,order=c(1,1),cond.dist="weibull")
m5=acd(adjdt,order=c(1,2),cond.dist="weibull")
ep5=m5$epsilon
acf(ep5,ylim=c(-0.05,0.25))
adt1=adjdt[1:1200]  # Subsample
plot(adt1,type='l')
m6=acd(adt1,order=c(1,1),cond.dist="weibull")
ep6=m6$epsilon
Box.test(ep6,lag=10,type='Ljung')
Box.test(ep6,lag=20,type='Ljung')
Box.test(ep6^2,lag=10,type='Ljung')
Box.test(ep6^2,lag=20,type='Ljung')
par(mfcol=c(2,1))
plot(ep6,type='l',xlab='index',ylab='epsilon_t')
acf(ep6,ylim=c(-0.1,.25)) 
####
source("hfanal.R")
# Process January data 
da=read.table("taq-cat-jan2010.txt",header=T)
m1=hfanal(da,1)
names(m1)
Ytot=m1$Ytot
Ntrad=m1$ntrad
Rv=cbind(Ytot,m1$realized)
m2=hfanal(da,2)
Rv=cbind(Rv,m2$realized)
m3=hfanal(da,3)
Rv=cbind(Rv,m3$realized)
m4=hfanal(da,4)
Rv=cbind(Rv,m4$realized)
m5=hfanal(da,5)
Rv=cbind(Rv,m5$realized)
m6=hfanal(da,10)
Rv=cbind(Rv,m6$realized)
m7=hfanal(da,15)
Rv=cbind(Rv,m7$realized)
m8=hfanal(da,20)
Rv=cbind(Rv,m8$realized)
m9=hfanal(da,30)
Rv=cbind(Rv,m9$realized)
## Process February data 
da=read.table("taq-cat-feb2010.txt",header=T)
m1=hfanal(da,1)
Rv2=cbind(m1$Ytot,m1$realized)
Ytot=c(Ytot,m1$Ytot)
Ntrad=c(Ntrad,m1$ntrad)
m2=hfanal(da,2)
Rv2=cbind(Rv2,m2$realized)
m3=hfanal(da,3)
Rv2=cbind(Rv2,m3$realized)
m4=hfanal(da,4)
Rv2=cbind(Rv2,m4$realized)
m5=hfanal(da,5)
Rv2=cbind(Rv2,m5$realized)
m6=hfanal(da,10)
Rv2=cbind(Rv2,m6$realized)
m7=hfanal(da,15)
Rv2=cbind(Rv2,m7$realized)
m8=hfanal(da,20)
Rv2=cbind(Rv2,m8$realized)
m9=hfanal(da,30)
Rv2=cbind(Rv2,m9$realized)
RV=rbind(Rv,Rv2)  # Combine Jan and Feb results
####
source("hf2ts.R")
da=read.table("taq-cat-may2010.txt",header=T)
m5=hf2ts(da,int=5)
names(m5)
da=read.table("taq-cat-apr2010.txt",header=T)
m4=hf2ts(da,int=5)
da=read.table("taq-cat-mar2010.txt",header=T)
m3=hf2ts(da,int=5)
da=read.table("taq-cat-feb2010.txt",header=T)
m2=hf2ts(da,int=5)
da=read.table("taq-cat-jan2010.txt",header=T)
m1=hf2ts(da,int=5)
# Combine the results
Ytot=c(m1$Ytot,m2$Ytot,m3$Ytot,m4$Ytot,m5$Ytot)  # Consecutive trades
# 2-scale method (Zhang et al. method)
RV=c(m1$realized,m2$realized,m3$realized,m4$realized,m5$realized) 
# average of 5-m RV. 
mRV=c(m1$ave.RV,m2$ave.RV,m3$ave.RV,m4$ave.RV,m5$ave.RV) 
