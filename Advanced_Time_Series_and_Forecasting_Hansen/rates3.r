# rates3.r #

library(quadprog)		# You may need to install the package #
library(tseries)		# You may need to install the package #
library(quantreg)		# You may need to install the package #

rates <- read.table("rates.txt")
rates <- as.matrix(rates)
n=nrow(rates)
year=as.matrix(rates[1:n,1])
month=as.matrix(rates[1:n,2])
t=year+(month-1)/12
r=as.matrix(rates[1:n,4])
dr=r[2:n]-r[1:(n-1)]
n=n-1

# Create Data Matrix Using 24 Initial Conditions @
kk=24			# Number of initial conditions #
nn=n-kk			# Number of data points less number of initial conditions equals number of observations #
y=as.matrix(dr[(1+kk):n])	# dependent variable #
x=matrix(1,nn+1,1)	# Regressors, first column (ones), one more observation than dependent variable (for forecast) #
for (j in 1:kk)
{ x=cbind(x,dr[(1+kk-j):(n-j+1)]) } 	# X matrix columns, lags of y #


# Model Combination #
kn=kk+1				# Number of models = AR(0) through AR(kk) #
yf=matrix(0,kn,1)		# vector of forecasts (empty for now) #
ee=matrix(0,nn,kn)		# matrix of prediction errors (empty for now) #

for (k in 1:kn)
{
  xk=x[1:nn,1:k]
  xf=x[nn+1,1:k]
  xxi=solve(t(xk)%*%xk)
  beta=xxi%*%(t(xk)%*%y)
  e=y-xk%*%beta
  h=rowSums((xk%*%xxi)*xk)
  eh=e/(1-h)
  yf[k]=xf%*%beta  
  ee[,k]=eh	
}

Dmat=(t(ee)%*%ee)/nn
dvec=matrix(0,kn,1)
Amat=t(rbind(matrix(1,1,kn),diag(kn)))
bvec=rbind(1,matrix(0,kn,1))
QP <- solve.QP(Dmat,dvec,Amat,bvec,bvec)
w <- QP$solution
w <- as.matrix(w)
e=ee%*%w
cv=t(w)%*%Dmat%*%w

yff=r[n+1]+t(yf)%*%w

x.arch <- garch(e,order=c(1,1))
archc=coef(x.arch)
sd=predict(x.arch)
like=logLik(x.arch)
sd <- as.matrix(sd[,1])
var <- as.matrix(sd^2)
sdf=sqrt(archc[1]+archc[2]*(e[nn]^2)+archc[3]*var[nn,1])

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

k=3
xk=x[1:nn,1:k]
xf=x[nn+1,1:k]
x2=xk[,2:k]
beta1=coef(rq(y~x2,.1))
beta2=coef(rq(y~x2,.25))
beta3=coef(rq(y~x2,.75))
beta4=coef(rq(y~x2,.9))
yf1=r[n+1]+xf%*%beta1
yf2=r[n+1]+xf%*%beta2
yf3=r[n+1]+xf%*%beta3
yf4=r[n+1]+xf%*%beta4

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
plot(yp,edf,main="Interest Rate Forecast Distribution",type="l",xlab="",ylab="")
