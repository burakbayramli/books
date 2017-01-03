# rates2.r #

library(quadprog)		# You may need to install the package #
library(tseries)		# You may need to install the package #

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

yff=t(yf)%*%w
print("Models, Weights")
print(cbind(seq(0,kk),w))
print("CV, Combination Forecast, Level Forecast")
print(cbind(cv,yff,r[n+1]+yff))

x.arch <- garch(e,order=c(1,1))
print(summary(x.arch))
archc=coef(x.arch)
sd=predict(x.arch)
like=logLik(x.arch)

var <- as.matrix(sd[,1]^2)
varf=archc[1]+archc[2]*(e[nn]^2)+archc[3]*var[nn,1]
print("Forecast Variance, Standard Deviation")
print(cbind(varf,sqrt(varf)))
print("Unconditional Variance, Standard Deviation")
sig=(t(e)%*%e)/nn
print(cbind(sig,sqrt(sig)))
print("Log Likelihood")
print(like)

t1 <- as.matrix(t[(kk+1):n])
e2=e^2
plot(t1,e,main="Leave-One-Out Prediction Residuals",type="l",xlab="",ylab="")
X11()
plot(t1,e2,main="Squared Prediction Residuals",type="l",xlab="",ylab="")
X11()
plot(t1,var,main="Estimated Variance",type="l",xlab="",ylab="")
