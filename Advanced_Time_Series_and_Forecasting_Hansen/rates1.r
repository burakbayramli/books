# rates1.r #

rates <- read.table("rates.txt")
rates <- as.matrix(rates)
n=nrow(rates)
year=as.matrix(rates[1:n,1])
month=as.matrix(rates[1:n,2])
t=year+(month-1)/12
r=as.matrix(rates[1:n,4])

plot(t,r,main="10-Year Treasury Bill Rate",type="l",xlab="",ylab="")

# First Difference #
dr=r[2:n]-r[1:(n-1)]
X11()
plot(t[2:n],dr,main="Change in 10-Year Rate",type="l",xlab="",ylab="")
n=n-1

# Create Data Matrix Using 24 Initial Conditions @
kk=24			# Number of initial conditions #
nn=n-kk			# Number of data points less number of initial conditions equals number of observations #
y=as.matrix(dr[(1+kk):n])	# dependent variable #
x=matrix(1,nn+1,1)	# Regressors, first column (ones), one more observation than dependent variable (for forecast) #
for (j in 1:kk)
{ x=cbind(x,dr[(1+kk-j):(n-j+1)]) } 	# X matrix columns, lags of y #


# Estimate AR(12) #
k=13			# Number of lags + 1 for intercept #
xk=x[1:nn,1:k]		# X matrix, for estimation	#
xf=x[nn+1,1:k]		# X vector, for forecast	#
xxi=solve(t(xk)%*%xk)	# X'X #
beta=xxi%*%(t(xk)%*%y)	# OLS #
e=y-xk%*%beta		# OLS residual #
h=rowSums((xk%*%xxi)*xk)	# leverage values #
eh=e/(1-h)			# LOO prediction residuals #
xe=xk*(eh%*%matrix(1,1,k))	# x*e #
omega=t(xe)%*%xe		# Central part of Variance matrix #
v=xxi%*%omega%*%xxi		# variance matrix estimator #
se=sqrt(diag(v))		# Standard errors #
betas=cbind(beta,se)		# estimates + standard errors #
yf=xf%*%beta			# Point forecast = Estimated change #
print("Last 4 values, level and change")
print(cbind(r[(n-2):(n+1)],dr[(n-3):n]))
print("AR(12) Model")
print("Coefficient Estimates and Standard Errors")
print(betas)
print("Point Forecast: Level and Change")
print(cbind(r[n+1]+yf,yf))

# Model Selection #
kn=kk+1				# Number of models = AR(0) through AR(kk) #
yf=matrix(nrow=kn,ncol=1)	# vector of forecasts (empty for now) #
crit=matrix(nrow=kn,ncol=7)	# vector of CV criterion (empty for now) #

# Estimate Full Model for Mallows Criteria #
  xk=x[1:nn,]
  xxi=solve(t(xk)%*%xk)
  e=y-xk%*%xxi%*%(t(xk)%*%y)
  h=rowSums((xk%*%xxi)*xk)
  ek=e/(1-h)
  sigk=(t(e)%*%e)/nn


for (k in 1:kn)
{
  xk=x[1:nn,1:k]
  xf=x[nn+1,1:k]
  xxi=solve(t(xk)%*%xk)
  beta=xxi%*%(t(xk)%*%y)
  e=y-xk%*%beta
  h=rowSums((xk%*%xxi)*xk)
  eh=e/(1-h)
  sig=(t(e)%*%e)/nn
  yf[k]=xf%*%beta  
  aic=nn*log(sig)+k*2			# AIC #
  xe=xk*(eh%*%matrix(1,1,k))		# x*e #
  omega=t(xe)%*%xe			# Central part of Variance matrix #
  b=matrix(1,1,k)%*%diag(xxi%*%omega)	# correction for Robust Mallows # 
 	
  crit[k,1]=nn*log(sig)+k*log(nn)	# BIC #
  crit[k,2]=aic				# AIC #
  crit[k,3]=aic+2*(k+1)*(k+2)/(nn-k-2)	# AICc #
  crit[k,4]=sig+2*k+sigk/nn		# Mallows #
  crit[k,5]=sig+2*b/nn			# Robust Mallows #
  crit[k,6]=sig+2*k+sig/nn		# FPE #
  crit[k,7]=(t(eh)%*%eh)/nn		# Cross-Validation #
}
k=which.min(crit[,7])


s=cbind(seq(0,kk),yf,crit[,1],crit[,3],crit[,5],crit[,7]) 
print("AR(0) through AR(24) Models, Point Forecasts, BIC, AICc, Robust Mallow, CV")
print(s)
print("Selected AR Order")
print(k-1)
print("Point Forecast: Level and Change")
print(cbind(r[n+1]+yf[k],yf[k]))
