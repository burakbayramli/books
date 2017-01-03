# Chap12.R

# Exhibit 12.1
win.graph(width=4.875, height=2.5,pointsize=8)
data(CREF)
plot(CREF)
t1=477;t2=508
polygon(x=c(t1,t1,t2,t2,t1),
y=c(min(CREF)-10,max(CREF)+10,max(CREF)+10,min(CREF)-10,min(CREF)-10),col='gray')
lines(CREF)


# Exhibit 12.2
r.cref=diff(log(CREF))*100
plot(r.cref)
t1=476;t2=507
polygon(x=c(t1,t1,t2,t2,t1),
y=c(-2,2.7,2.7,-2,-2),col='gray')
lines(r.cref)
abline(h=0)

# Exhibit 12.3
acf(r.cref)

# Exhibit 12.4
pacf(r.cref)

# Exhibit 12.5
acf(abs(r.cref))

# Exhibit 12.6
pacf(abs(r.cref))

# Exhibit 12.7
acf(r.cref^2)

# Exhibit 12.8
pacf(r.cref^2)

# Exhibit 12.9
win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=r.cref)

# Exhibit 12.10
qqnorm(r.cref)
qqline(r.cref)

# Implement the Jarque-Bera test for normality in two different ways
skewness(r.cref)
kurtosis(r.cref)
length(r.cref)*skewness(r.cref)^2/6
length(r.cref)*kurtosis(r.cref)^2/24
JB=length(r.cref)*(skewness(r.cref)^2/6+kurtosis(r.cref)^2/24)
JB # The Jarque-Bera test statistic
1-pchisq(JB,df=2)


library(tseries)
jarque.bera.test(r.cref)


# Exhibit 12.11
set.seed(1235678)
garch01.sim=garch.sim(alpha=c(.01,.9),n=500)
plot(garch01.sim,type='l',ylab=expression(r[t]),xlab='t')

# Exhibit 12.12
set.seed(1234567)
garch11.sim=garch.sim(alpha=c(0.02,0.05),beta=.9,n=500)
plot(garch11.sim,type='l',ylab=expression(r[t]),xlab='t')

# Exhibit 12.13
acf(garch11.sim)

# Exhibit 12.14
pacf(garch11.sim)

# Exhibit 12.15
acf(abs(garch11.sim))

# Exhibit 12.16
pacf(abs(garch11.sim))

# Exhibit 12.17
acf(garch11.sim^2)

# Exhibit 12.18
pacf(garch11.sim^2)

# Exhibit 12.19
eacf((garch11.sim)^2)

# Exhibit 12.20
eacf(abs(garch11.sim))

# Exhibit 12.21
eacf(abs(r.cref))

# Exhibit 12.22
arima(abs(abs(garch11.sim)),order=c(1,0,1))

# Exhibit 12.23
g1=garch(garch11.sim,order=c(2,2))
summary(g1)

# Exhibit 12.24
g2=garch(garch11.sim,order=c(1,1))
summary(g2)

# Exhibit 12.25
m1=garch(x=r.cref,order=c(1,1))
summary(m1)

# Exhibit 12.26
plot(residuals(m1),type='h',ylab='standardized residuals')

# Exhibit 12.27
qqnorm(residuals(m1))
qqline(residuals(m1))

# Exhibit 12.28
acf(residuals(m1)^2,na.action=na.omit)

# Exhibit 12.29
gBox(m1,method='squared')


gBox(m1,lags=20, plot=F,method='squared')$pvalue
# Exhibit 12.30
acf(abs(residuals(m1)),na.action=na.omit)

# Overfitting the GARCH(1,2) model to the CREF returns
m2=garch(x=r.cref,order=c(1,2))
summary(m2,diagnostics=F) # The summary is based on the summary.garch function
# in the tseries pacakge. Note the Ljung-Box test from the summary is not
# valid; see the text book. 
AIC(m1)
AIC(m2)
 

# Exhibit 12.31
gBox(m1,method='absolute')

#Further model diagnostic of the fitted GARCH(1,1) model for the CREF returns
shapiro.test(na.omit(residuals(m1)))
jarque.bera.test(na.omit(residuals(m1)))
skewness(na.omit(residuals(m1)))
kurtosis(na.omit(residuals(m1)))

# Exhibit 12.32
plot((fitted(m1)[,1])^2,type='l',ylab='conditional variance',xlab='t')


# Exhibit 12.33
data(usd.hkd)
plot(ts(usd.hkd$hkrate,freq=1),type='l',xlab='day',ylab='return')
abline(v=203,lwd=2.5,col="gray")
lines(ts(usd.hkd$hkrate,freq=1))

# Exhibit 12.34
attach(usd.hkd)
McLeod.Li.test(arima(hkrate,order=c(1,0,0), xreg=data.frame(outlier1)))

# Exhibit 12.36
plot(ts(usd.hkd$v,freq=1),type='l',xlab='day', ylab='conditional variance')


# Exhibit 12.35 and 12.37 are obtained by running programs in SAS, part of
#the code are exhibited below.

data hkex;
infile "hkrate.dat";
input hkrate;
outlier1=0;
outlier2=0;
day+1;
if day=203 then outlier1=1;
if day=290 then outlier2=1;
proc autoreg data=hkex;
        model hkrate=outlier1 /noint  nlag=1 garch=(p=3,q=1) maxiter=200 archtest;
		/*hetero outlier /link=linear;*/
		output out=a cev=v residual=r;	
run;

