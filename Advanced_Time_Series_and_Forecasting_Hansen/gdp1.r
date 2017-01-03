# gdp1.r #

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

plot(t,gdp,main="GDP",type="l",xlab="",ylab="")

X11()
plot(t,gdpg,main="GDP Growth",type="l",xlab="",ylab="")

X11()
plot(t,t10,main="Treasury Bill Rates",type="l",xlab="",ylab="")
lines(t,t3)

X11()
plot(t,spread,main="Spread Between 10-Year and 3-Month",type="l",xlab="",ylab="")

X11()
plot(t,aaa,main="Corporate Bond Rates",type="l",xlab="",ylab="")
lines(t,baa)

X11()
plot(t,dspread,main="High-Yield Spread",type="l",xlab="",ylab="")

X11()
plot(t,hs,main="Housing Starts, Building Permits",type="l",xlab="",ylab="")
lines(t,bp)


# Create Data Matrix Using 12 Initial Conditions @
kk=12			# Number of initial conditions #
nn=n-kk			# Number of data points less number of initial conditions equals number of observations #
y=as.matrix(gdpg[(1+kk):n])	# dependent variable #
x=matrix(1,nn+1,1)	# Regressors, first column (ones), one more observation than dependent variable (for forecast) #
for (j in 1:kk)
{ x=cbind(x,gdpg[(1+kk-j):(n-j+1)]) } 	# X matrix columns, lags of y #


# Estimate AR(4) #
k=5			# Number of lags + 1 for intercept #
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
print("Last 5 values")
print(gdpg[(n-4):n])
print("AR(4) Model")
print("Coefficient Estimates and Standard Errors")
print(betas)
print("Point Forecast")
print(yf)


# Model Selection #
kn=kk+1				# Number of models = AR(0) through AR(kk) #
yf=matrix(nrow=kn,ncol=1)	# vector of forecasts (empty for now) #
crit=matrix(nrow=kn,ncol=7)	# matrix for selection criterion (empty for now) #

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

# Report Selected AR Model #

xk=x[1:nn,1:k]
xf=x[nn+1,1:k]
xxi=solve(t(xk)%*%xk)
beta=xxi%*%(t(xk)%*%y)
e=y-xk%*%beta
h=rowSums((xk%*%xxi)*xk)
eh=e/(1-h)
sig=(t(e)%*%e)/nn
xe=xk*(eh%*%matrix(1,1,k))	# x*e #
omega=t(xe)%*%xe		# Central part of Variance matrix #
v=xxi%*%omega%*%xxi		# variance matrix estimator #
se=sqrt(diag(v))		# Standard errors #
betas=cbind(beta,se)		# estimates + standard errors #

print("Selected AR Model")
print("Selected AR Order")
print(k-1)
print("Point Forecast")
print(yf[k])
print("Coefficient Estimates and Standard Errors")
print(betas)


# Leading Indicator Forecasting MOdel #
x1=x[1:(nn+1),1:k]		# Keep selected AR model Variables #
xs=cbind(spread[(kk):n],dspread[(kk):n],hs[(kk):n],bp[(kk):n]) # Leading Indicators #

xn=ncol(xs)
s=seq(1,xn)		# column indicators for xs #
			# We now create a matrix, where each row indicates which elements of xs to include in a model #
			# Each row is a model #
			# The number of columns is the same as xs #
models1=c(		
1,0,0,0,
0,1,0,0,
0,0,1,0,
0,0,0,1,
1,1,0,0,
1,0,1,0,
1,0,0,1,
0,1,1,0,
0,1,0,1,
0,0,1,1,
1,1,1,0,
1,1,0,1,
1,0,1,1,
0,1,1,1,
1,1,1,1)

jj=length(models1)/xn			# number of models #
# matrix of variables to select #
models=matrix(models1,nrow=jj,ncol=xn,byrow=1)

yf=matrix(nrow=jj,ncol=1)	# vector of forecasts (empty for now) #
cv=matrix(nrow=jj,ncol=1)	# vector for cv criteria (empty for now) #

				# Calculate CV for each model #
for (j in 1:jj){
  ji=s[models[j,]==1]
  xj=cbind(x1,xs[,ji])
  xk=xj[1:nn,]
  xf=xj[nn+1,]
  xxi=solve(t(xk)%*%xk)
  beta=xxi%*%(t(xk)%*%y)
  e=y-xk%*%beta
  h=rowSums((xk%*%xxi)*xk)
  eh=e/(1-h)
  cv[j]=(t(eh)%*%eh)/nn
  yf[j]=xf%*%beta  
}

j=which.min(cv)			# minimizing CV #
  ji=s[models[j,]==1]
  xj=cbind(x1,xs[,ji])
  xk=xj[1:nn,]
  xf=xj[nn+1,]
  xxi=solve(t(xk)%*%xk)
  beta=xxi%*%(t(xk)%*%y)
  e=y-xk%*%beta
  h=rowSums((xk%*%xxi)*xk)
  eh=e/(1-h)
xe=xk*(eh%*%matrix(1,nrow=1,ncol=ncol(xk)))
omega=t(xe)%*%xe
v=xxi%*%omega%*%xxi
se=sqrt(diag(v))
betas=cbind(beta,se)
mm=cbind(models,cv,yf);

print("Leading Indicator Model")
print("Models, CV, and forecast")
print(mm)
print("Selected Variables")
print(ji)
print("Coefficient Estimates and Standard Errors")
print(betas)
print("Point Forecast")
print(yf[j])
