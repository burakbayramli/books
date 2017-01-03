# gdp3.r #

library(quadprog)		# You may need to install the package #
library(tseries)		# You may need to install the package #
library(quantreg)		# You may need to install the package #

gdpdata <- read.table("gdpdata.txt")
gdpdata <- as.matrix(gdpdata)
n=nrow(gdpdata)
year=as.matrix(gdpdata[1:n,1])
quarter=as.matrix(gdpdata[1:n,2])
t=year+(quarter-1)/4

gdp=as.matrix(gdpdata[1:n,3])
gdpg=as.matrix(gdpdata[1:n,4])
t10=as.matrix(gdpdata[1:n,5])
t3=as.matrix(gdpdata[1:n,6])
aaa=as.matrix(gdpdata[1:n,7])
baa=as.matrix(gdpdata[1:n,8])
hs=as.matrix(gdpdata[1:n,9])
bp=as.matrix(gdpdata[1:n,10])
spread=t10-t3
dspread=baa-aaa

# Create Data Matrix Using 12 Initial Conditions @
kk=12			# Number of initial conditions #
nn=n-kk			# Number of data points less number of initial conditions equals number of observations #
y=as.matrix(gdpg[(1+kk):n])	# dependent variable #
x=matrix(1,nn+1,1)	# Regressors, first column (ones), one more observation than dependent variable (for forecast) #
for (j in 1:kk)
{ x=cbind(x,gdpg[(1+kk-j):(n-j+1)]) } 	# X matrix columns, lags of y #

# Leading Indicator Forecasting MOdel #
xs=cbind(x[1:(nn+1),1:3],spread[(kk):n],dspread[(kk):n],hs[(kk):n],bp[(kk):n]) # Leading Indicators #

xn=ncol(xs)
s=seq(1,xn)		# column indicators for xs #
			# We now create a matrix, where each row indicates which elements of xs to include in a model #
			# Each row is a model #
			# The number of columns is the same as xs #
models1=c(		
1,1,0,0,0,0,0,
1,1,0,1,0,0,0,
1,1,0,0,1,0,0,
1,1,0,0,0,1,0,
1,1,0,0,0,0,1,
1,1,0,1,1,0,0,
1,1,0,1,0,1,0,
1,1,0,1,0,0,1,
1,1,0,0,1,1,0,
1,1,0,0,1,0,1,
1,1,0,0,0,1,1,
1,1,0,1,1,1,0,
1,1,0,1,1,0,1,
1,1,0,1,0,1,1,
1,1,0,0,1,1,1,
1,1,0,1,1,1,1,
1,1,1,0,0,0,0,
1,1,1,1,0,0,0,
1,1,1,0,1,0,0,
1,1,1,0,0,1,0,
1,1,1,0,0,0,1,
1,1,1,1,1,0,0,
1,1,1,1,0,1,0,
1,1,1,1,0,0,1,
1,1,1,0,1,1,0,
1,1,1,0,1,0,1,
1,1,1,0,0,1,1,
1,1,1,1,1,1,0,
1,1,1,1,1,0,1,
1,1,1,1,0,1,1,
1,1,1,0,1,1,1,
1,1,1,1,1,1,1
)

jj=length(models1)/xn			# number of models #
# matrix of variables to select #
models=matrix(models1,nrow=jj,ncol=xn,byrow=1)

yf=matrix(0,jj,1)			# vector of forecasts (empty for now) #
ee=matrix(0,nn,jj)		# matrix of prediction errors (empty for now) #
				
for (j in 1:jj){
  ji=s[models[j,]==1]
  xj=xs[,ji]
  xk=xj[1:nn,]
  xf=xj[nn+1,]
  xxi=solve(t(xk)%*%xk)
  beta=xxi%*%(t(xk)%*%y)
  e=y-xk%*%beta
  h=rowSums((xk%*%xxi)*xk)
  eh=e/(1-h)
  ee[,j]=eh
  yf[j]=xf%*%beta  
}

Dmat=(t(ee)%*%ee)/nn
dvec=matrix(0,jj,1)
Amat=t(rbind(matrix(1,1,jj),diag(jj)))
bvec=rbind(1,matrix(0,jj,1))
QP <- solve.QP(Dmat,dvec,Amat,bvec,bvec)
w <- QP$solution
w <- as.matrix(w)
e=ee%*%w
yff=t(yf)%*%w
cv=t(w)%*%Dmat%*%w
sig=(t(e)%*%e)/nn

st=c(.8,.2,.7)
x.arch <- garch(e,order=c(1,1),control=garch.control(start=st))
archc=coef(x.arch)
sd=predict(x.arch)
sd <- as.matrix(sd[,1])
sdf=sqrt(archc[1]+archc[2]*(e[nn]^2)+archc[3]*(sd[nn]^2))

# Quantile Error Calculation #

ep=e/sd
print(summary(ep))
q1=coef(rq(ep~1,.1))
q2=coef(rq(ep~1,.25))
q3=coef(rq(ep~1,.75))
q4=coef(rq(ep~1,.9))
i1=yff+sdf*q1
i2=yff+sdf*q2
i3=yff+sdf*q3
i4=yff+sdf*q4

print("10%, 25%, 75%, 90% quantiles of normalized residuals")
print(cbind(q1,q2,q3,q4))
print("Point Forecast")
print(yff)
print("50% Forecast Interval")
print(cbind(i2,i3))
print("80% Forecast Interval")
print(cbind(i1,i4))

# Quantile Regression approach to forecast intervals #
  j=28					# Use AR(2) plus first 3 regressors #
  ji=s[models[j,]==1]
  xj=xs[,ji]
  xk=xj[1:nn,]
  xf=xj[nn+1,]
  x2=xk[,2:ncol(xk)]
beta1=coef(rq(y~x2,.1))
beta2=coef(rq(y~x2,.25))
beta3=coef(rq(y~x2,.75))
beta4=coef(rq(y~x2,.9))
yf1=xf%*%beta1
yf2=xf%*%beta2
yf3=xf%*%beta3
yf4=xf%*%beta4

print("Quantile Regression Intervals")
print("50% Forecast Interval")
print(cbind(yf2,yf3))
print("80% Forecast Interval")
print(cbind(yf1,yf4))
print("Estimated Coefficients")
print(beta1)
print(beta2)
print(beta3)
print(beta4)

# Forecast Distribution #
e1=as.matrix(sort(ep))				# Sort the normalized residuals #
n1=nrow(e1)
yp=matrix(1,n1,1)%*%yff+e1%*%sdf		# Forecast Distribution #
edf=seq(1,n1)/n1
plot(yp,edf,main="GDP Forecast Distribution",type="l",xlab="",ylab="")

# Multi-Step Forecasts #

hn=8
k=ncol(xk)
yfh=matrix(1,hn,1)
yf1=matrix(1,hn,1)
yf2=matrix(1,hn,1)
yf3=matrix(1,hn,1)
yf4=matrix(1,hn,1)

yg=400*log(gdp)
for (h in 1:hn){
 nh=nn+1-h		# Number of observations #
 xh=xk[1:nh,]
 yh=(yg[(n-nh+1):n]-yg[(n-nh+1-h):(n-h)])/h
 xxi=solve(t(xh)%*%xh)
 beta=xxi%*%(t(xh)%*%yh)
 yfh[h]=xf%*%beta 
 e=yh-xh%*%beta
 eh=e			# Compute Leave-h-out prediction errors #
 for (i in 1:nh){
   ii=seq(i-h+1,i+h-1)
   ii<-ii[ii>0]
   yi=yh[-ii]
   xi=xh[-ii,]
   betai=solve(t(xi)%*%xi)%*%(t(xi)%*%yi)
   eh[i]=yh[i]-xh[i,]%*%betai
 }
 xe=xh*(eh%*%matrix(1,1,k))	# x*e #
 omega=t(xe)%*%xe		# HAC computation #
 if (h>1) {			
   for (j in 1:(h-1)){
    gm=t(xe[(1+j):nh,])%*%xe[1:(nh-j),]
    omega=omega+gm+t(gm)
   }
 }
 v=xxi%*%omega%*%xxi
 se=sqrt(diag(v))		# Standard errors #
 betas=cbind(beta,se)		# estimates + standard errors #
 print("Forecast Horizon: Coefficients")
 print(h)
 print(betas)

# Quantile Regression Intervals #
x2=xh[,2:k]
beta1=coef(rq(yh~x2,.1))
beta2=coef(rq(yh~x2,.25))
beta3=coef(rq(yh~x2,.75))
beta4=coef(rq(yh~x2,.9))
yf1[h]=xf%*%beta1
yf2[h]=xf%*%beta2
yf3[h]=xf%*%beta3
yf4[h]=xf%*%beta4


}

print("Forecasts and 80% intervals")
print(cbind(seq(1,h),yfh,yf1,yf4))

# Create Fan Chart #

yy=as.matrix(y[193:197])
yt=rbind(yy,yfh)
yt1=rbind(yy,yf1)
yt2=rbind(yy,yf2)
yt3=rbind(yy,yf3)
yt4=rbind(yy,yf4)

tt=seq(2011,2014,by=.25)
plot(tt,yt,type="l",ylim=c(-1,5),main="",xlab="",ylab="")
lines(tt,yt1)
lines(tt,yt2)
lines(tt,yt3)
lines(tt,yt4)
