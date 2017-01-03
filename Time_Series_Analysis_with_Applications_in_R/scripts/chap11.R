# chap11.R

# Exhibit 11.1

win.graph(width=4.875, height=2.5,pointsize=8)
data(airmiles)
plot(log(airmiles),ylab='Log(airmiles)',xlab='Year', )

# Exhibit 11.5
acf(as.vector(diff(diff(window(log(airmiles),end=c(2001,8)),12))),lag.max=48)

# Exhibit 11.6 
air.m1=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),
period=12),xtransf=data.frame(I911=1*(seq(airmiles)==69),
I911=1*(seq(airmiles)==69)),
transfer=list(c(0,0),c(1,0)),xreg=data.frame(Dec96=1*(seq(airmiles)==12),
Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),method='ML')
# Additive outliers are incorporated as dummy variables in xreg.
# Transfer function components are incorporated by the xtransf and transfer
# arguments.
# Here, the transfer function consists of two parts omega0*P(t) and 
# omega1/(1-omega2*B)P(t) where the inputs of the two transfer
# functions are identical and equals the dummy variable that is 1 at September
# 2001 (the 69th data point) and zero otherwise.
# xtransf is a matrix whose columns are the input variables.
# transfer is a list consisting of the pair of (MA order, AR order) of each
# transfer function, which in this examples is (0,0) and (1,0).

air.m1

# Exhibit 11.7
plot(log(airmiles),ylab="log(airmiles)")
points(fitted(air.m1))


# Exhibit 11.8
Nine11p=1*(seq(airmiles)==69)
plot(ts(Nine11p*(-0.0949)+
filter(Nine11p,filter=.8139,method='recursive',side=1)*(-0.2715),
frequency=12,start=1996),type='h',ylab='9/11 Effects')
abline(h=0)

# Exhibit 11.9 
set.seed(12345)
y=arima.sim(model=list(ar=.8,ma=.5),n.start=158,n=100)
y[10]
y[10]=10
y=ts(y,freq=1,start=1)
plot(y,type='o')
acf(y)
pacf(y)
eacf(y)
m1=arima(y,order=c(1,0,0))
m1
detectAO(m1)
detectAO(m1, robust=F)
detectIO(m1)
m2=arima(y,order=c(1,0,0),xreg=data.frame(AO=seq(y)==10))
m2
detectAO(m2)
detectIO(m2)
tsdiag(m2)
tsdiag(m1)
m3=arima(y,order=c(1,0,1),xreg=data.frame(AO=seq(y)==10))
detectAO(m3)
detectIO(m3)
tsdiag(m3)
m3

plot(y,type='b') 
arrows(30,6, 11,9.8, length=.1,angle=20)
text(34,6.2, "AO")


# Exhibit 11.10
data(co2)
m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2
detectAO(m1.co2)
detectIO(m1.co2)
m4.co2=arimax(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),
io=c(57))
m4.co2

# the signs of the MA coefficient estimates appear to be opposite to 
# those in the book, because R uses the plus convention in the MA parameterization.

# Exhibit 11.11
win.graph(width=4.875, height=2.5,pointsize=8)
set.seed(12345)
X=rnorm(105)
Y=zlag(X,2)+.5*rnorm(105)
X=ts(X[-(1:5)],start=1,freq=1)
Y=ts(Y[-(1:5)],start=1,freq=1)
ccf(X,Y,ylab='CCF')


# Exhibit 11.12
phi=seq(0,.95,.15)
rejection=2*(1-pnorm(1.96*sqrt((1-phi^2)/(1+phi^2))))
M=signif(rbind(phi,rejection),2)
rownames(M)=c("phi", "Error Rate")
M


# Exhibit 11.13
set.seed(23457)
correlation.v=NULL
B=1000
n=500
for (i in 1:B) {x=cumsum(arima.sim(model=list(ma=.8),n=n))
y=cumsum(arima.sim(model=list(ma=.8),n=n))
correlation.v=c(correlation.v,ccf(x,y,lag.max=1,plot=F)$acf[2])
}
hist(correlation.v,prob=T,xlab=expression(r[0](X,Y)))

# Exhibit 11.14
data(milk)
data(electricity)
milk.electricity=ts.intersect(milk,log(electricity))
# The ts.intersect function merges serveral time series into a panel of time
# series over the time frame where each series has data.

plot(milk.electricity,yax.flip=T) 
# the option yax.flip=T flips the y-axis for the series alternately so as
# to make the labeling clearer.

# Exhibit 11.15
ccf(as.numeric(milk.electricity[,1]),as.numeric(milk.electricity[,2]),
main='milk & electricity',ylab='CCF')
# The as.numeric function strips the time series attribute from the time series.
# This is done to nullify the default way the ccf function plots the cross-correlations.
# You may want to repeat the command without the as.numeric function to see 
# the default labels of the lags according to the period of the data.
# ccf((milk.electricity[,1]),(milk.electricity[,2]), main='milk & electricity',ylab='CCF')


# Exhibit 11.16
me.dif=ts.intersect(diff(diff(milk,12)),diff(diff(log(electricity),12)))
prewhiten(as.numeric(me.dif[,1]),as.numeric(me.dif[,2]),
,ylab='CCF' )

# Exhibit 11.17
data(bluebird)
plot(bluebird,yax.flip=T)

# Exhibit 11.18
prewhiten(y=diff(bluebird)[,1],x=diff(bluebird)[,2],main="Price & Sales",ylab='CCF')
# As the time series are of unit period, there is no need to apply the as.numeric 
# function.

# Exhibit 11.19
sales=bluebird[,1]
price=bluebird[,2]
chip.m1=lm(sales~price,data=bluebird)
summary(chip.m1)

# Exhibit 11.20
acf(residuals(chip.m1),ci.type='ma')

# Exhibit 11.21
pacf(residuals(chip.m1))

# Exhibit 11.22
eacf(residuals(chip.m1))

# Exhibit 11.23
chip.m2=arima(sales,order=c(1,0,4),xreg=data.frame(price))
chip.m2
# MA(1)& MA(3) estimates are not significant, at 5% level,
# so they are constrained to be zero in the model fit below.
chip.m3=arima(sales,order=c(1,0,4),xreg=data.frame(price),fixed=c(NA,0,NA,0,NA,NA,NA))
# The MA(1) & MA(3) estimates are the second and fourth coefficients listed
# in the model fit chip.m2. They are set to be zero using the fixed option.
# NAs in the fixed option correspond to free parameters. 
chip.m3
# Now, the AR(1) coefficient estimate also seems not significant, so it is
# removed in the next fitted model.
chip.m4=arima(sales,order=c(0,0,4),xreg=data.frame(price),fixed=c(0,NA,0,NA,NA,NA))
chip.m4

# model diagnostic can be done by running the tsdiag command.
tsdiag(chip.m4)


# Exhibit 11.24
data(boardings)
plot(boardings,yax.flip=T)
# The Denver dataset has three time series. Here, we only plot the first 
# two series.

# Exhibit 11.25
m1=arima(boardings[,2],order=c(2,1,0))
prewhiten(x=boardings[,2],y=boardings[,1],x.model=m1)


# Exhibit 11.26
log.boardings=boardings[,1]
log.price=boardings[,2]
boardings.m1=arima(log.boardings,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12),
xreg=data.frame(log.price))
boardings.m1
detectAO(boardings.m1) 
detectIO(boardings.m1)
# An AO is detected at time point 32, as well as an IO detected at time point 44
# Since the test statistic of the AO has larger magnitude, an AO is added to the
# model fitted below.
boardings.m2=arima(log.boardings,order=c(1,0,3),seasonal=list(order=c(1,0,0),period=12),
xreg=data.frame(log.price,outlier=c(rep(0,31),1,rep(0,36))),
fixed=c(NA,0,0,rep(NA,5)))
boardings.m2
detectAO(boardings.m2)
detectIO(boardings.m2)
# No outliers are detected!
tsdiag(boardings.m2,tol=.15,gof.lag=24)
# Model diagnostics appear to be OK.




