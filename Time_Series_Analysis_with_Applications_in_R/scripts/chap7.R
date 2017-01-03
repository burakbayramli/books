# Chap7.R

# Below is a function that computes the method of moments estimator of
# the MA(1) coefficient of an MA(1) model.
estimate.ma1.mom=function(x){r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) 
return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}

# Exhibit 7.1
data(ma1.2.s)
estimate.ma1.mom(ma1.2.s)

data(ma1.1.s)
estimate.ma1.mom(ma1.1.s)

set.seed(1234)
ma1.3.s=arima.sim(list(ma=c(.9)),n=60)
estimate.ma1.mom(ma1.3.s)

ma1.4.s=arima.sim(list(ma=c(-0.5)),n=60) 
estimate.ma1.mom(ma1.4.s)

arima(ma1.4.s,order=c(0,0,1),method='CSS',include.mean=F)
data(ar1.s)
ar(ar1.s,order.max=1,AIC=F,method='yw')
data(ar1.2.s)
ar(ar1.2.s,order.max=1,AIC=F,method='yw')
data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw')


# Exhibit 7.4
data(ar1.s)
ar(ar1.s,order.max=1,AIC=F,method='yw') # method of moments
ar(ar1.s,order.max=1,AIC=F,method='ols') # conditional sum of squares
ar(ar1.s,order.max=1,AIC=F,method='mle') # maximum likelihood
# The AIC option is set to be False otherwise the function will choose
# the AR order by minimizing AIC, so that zero order might be chosen.

data(ar1.2.s)
ar(ar1.2.s,order.max=1,AIC=F,method='yw') # method of moments
ar(ar1.2.s,order.max=1,AIC=F,method='ols') # conditional sum of squares
ar(ar1.2.s,order.max=1,AIC=F,method='mle') # maximum likelihood

# Exhibit 7.5
data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw') # method of moments
ar(ar2.s,order.max=2,AIC=F,method='ols') # conditional sum of squares
ar(ar2.s,order.max=2,AIC=F,method='mle') # maximum likelihood

# Exhibit 7.6
data(arma11.s)
arima(arma11.s, order=c(1,0,1),method='CSS') # conditional sum of squares
arima(arma11.s, order=c(1,0,1),method='ML') # maximum likelihood
# 
# Recall that R uses the plus convention whereas our book uses the minus 
# convention in the specification of the MA part, i.e. R specifies an
# ARMA(1,1) model as z_t=theta_0+phi*z_{t-1}+e_t+theta_1*e_{t-1} 
# versus our convention
# z_t=theta_0+phi*z_{t-1}+e_t-theta_1*e_{t-1} 

# Exhibit 7.7
data(color)
ar(color,order.max=1,AIC=F,method='yw') # method of moments
ar(color,order.max=1,AIC=F,method='ols') # conditional sum of squares
ar(color,order.max=1,AIC=F,method='mle') # maximum likelihood


# Exhibit 7.8
data(hare)
arima(sqrt(hare),order=c(3,0,0))

# Exhibit 7.9
data(oil.price)
arima(log(oil.price),order=c(0,1,1),method='CSS') # conditional sum of squares
arima(log(oil.price),order=c(0,1,1),method='ML') # maximum likelihood

# Exhibit 7.10

res=arima(sqrt(hare),order=c(3,0,0),include.mean=T)
set.seed(12345)
coefm.cond.norm=arima.boot(res,cond.boot=T,is.normal=T,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method I

coefm.cond.replace=arima.boot(res,cond.boot=T,is.normal=F,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method II

coefm.norm=arima.boot(res,cond.boot=F,is.normal=T,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method III

coefm.replace=arima.boot(res,cond.boot=F,is.normal=F,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method IV

# Some bootstrap series may be explosive which will be discarded. To see
# the number of usable bootstrap series, run the command

dim(coefm.replace)
# the output should be 
# [1] 952   5
# i.e. we have only 952 usable (i.e. finite) bootstrap time series even though
# we simulate 1000 series.

# The theoretical confidence intervals were computed by the output in Exhibit 7.8.


# Compute the quasi-period of the bootstrap series based on the method of
# stationary bootstrap with the errors drawn from the residuals with replacement.

period.replace=apply(coefm.replace,1,function(x){
roots=polyroot(c(1,-x[1:3]))
# find the complex root with smalles magnitude
min1=1.e+9
rootc=NA
for (root in roots) {
if( abs(Im(root))<1e-10) next
if (Mod(root)< min1) {min1=Mod(root); rootc=root}
}
if(is.na(rootc)) period=NA else period=2*pi/abs(Arg(rootc))
period
})

sum(is.na(period.replace)) # number of bootstap series that do not admit a  well-defined quasi-period.

quantile(period.replace, c(.025,.975),na.rm=T)

# Exhibit 7.11
win.graph(width=3.9,height=3.8,pointsize=8)
hist(period.replace,prob=T,main="",xlab="quasi-period",axes=F,xlim=c(5,16))
axis(2)
axis(1,c(4,6,8,10,12,14,16),c(4,6,8,10,12,14,NA))


# Exhibit 7.12

win.graph(width=3,height=3,pointsize=8)
qqnorm(period.replace,main="") #Normal Q-Q Plot for the Bootstrap Quasi-period Estimates")
qqline(period.replace)





