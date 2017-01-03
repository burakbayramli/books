# gdp2.r #

library(quadprog)		# You may need to install the package #
library(tseries)		# You may need to install the package #

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
print("CV, Combination Forecast")
print(cbind(cv,yff))


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

print("Model, Weights")
print(cbind(models,w))
print("CV, Combination Forecast")
print(cbind(cv,yff))

st=c(.8,.2,.7)
x.arch <- garch(e,order=c(1,1),control=garch.control(start=st))
print(summary(x.arch))
archc=coef(x.arch)
sd=predict(x.arch)
like=logLik(x.arch)

var <- as.matrix(sd[,1]^2)
varf=archc[1]+archc[2]*(e[nn]^2)+archc[3]*var[nn,1]
print("Forecast Variance, Standard Deviation")
print(cbind(varf,sqrt(varf)))
print("Unconditional Variance, Standard Deviation")
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
