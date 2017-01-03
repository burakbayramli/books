# chap10.R

# Exhibit 10.1
library(TSA)
data(co2)
# The stats pacakage has another dataset of the same name 'co2', so
# make sure that you have loaded the TSA package before loading the co2 data.
win.graph(width=4.875, height=3,pointsize=8)
plot(co2,ylab='CO2',main='Monthly Carbon Dioxide Levels at Alert, NWT, Canada')


# Exhibit 10.2
plot(window(co2,start=c(2000,1)),main='Carbon Dioxide Levels with Monthly Symbols', ylab='CO2')
# specify the y-label as "CO2", otherwise it will use "window(co2,start=c(2000,1))"
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(co2,start=c(2000,1)),pch=Month)

# Exhibit 10.3

# First divide the plotting area into a 1 by 2 matrix of plots, i.e. one row
# of two figures. 
win.graph(width=4.875, height=3,pointsize=8)
par(mfrow=c(1,2))
# The ARMAacf function computes the theoretical acf of an ARMA model.
# Note that the seasonal MA part (1+0.5B)(1+0.8B**12)=(1+0.5B+0.8B**12+0.4B**13).
plot(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(0,0.6))
points(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.5,labels=expression(list(theta==-0.5,Theta==-0.8)))

plot(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F)
points(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.35,labels=expression(list(theta==0.5,Theta==-0.8)))


# Exhibit 10.4
plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[-1],x=1:61,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8),xlim=c(0,61))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[c(1,11,12,13,
23,24,25,35,36,37,47,48,49,59,60,61)+1],
x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==-0.4)))

plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[-1],x=1:61,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[c(1,11,12,13,
23,24,25,35,36,37,47,48,49,59,60,61)+1],
x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==0.4)))

# Exhibit 10.5
par(mfrow=c(1,1))
acf(as.vector(co2),lag.max=36,
main=expression(Sample~~ACF~~of~~CO[2]~~Levels))

# Exhibit 10.6
plot(diff(co2),main=expression(Time~~Series~~Plot~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels), ylab=expression(First~~Difference~~of~~CO[2])) 

# Exhibit 10.7
acf(as.vector(diff(co2)),lag.max=36,
main=expression(Sample~~ACF~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels))

# Exhibit 10.8
plot(diff(diff(co2),lag=12),main=expression(Time~~Series~~Plot~~of~~the~~First~~and~~
Seasonal~~Differences~~of~~CO[2]~~Levels), 
ylab=expression(First~~and~~Seasonal~~Difference~~of~~C~O[2])) 

# Exhibit 10.9
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma',
main=expression(Sample~~ACF~~of~~the~~First~~and~~Seasonal~~Differences~~of~~
CO[2]~~Levels))

# Exhibit 10.10
# Do remember that in the book the MA parameterization uses the minus convention but R uses
# the positive convention, lest you find the estimates from R to be different from the reported values
# in the book!
m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2

# Exhibit 10.11
# first thirteen residuals are omitted from the plot.
plot(window(rstandard(m1.co2),start=c(1995,2)),ylab='Standardized Residuals', type='o',
main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))
abline(h=0)

# Exhibit 10.12
acf(as.vector(window(rstandard(m1.co2),start=c(1995,2))),lag.max=36,
main=expression(ACF~~of~~Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

# The figures in the above two exhibits can also be obtained as the first two sub-plots from the
# following command. The plotting convention is slightly different for the first sub-plot.
win.graph(width=4.875, height=5,pointsize=8)
# by default, the first 13 residuals are ommited; please use ?tsdiag to learn
# more about the function.
tsdiag(m1.co2, gof.lag=36)

# Exhibit 10.13
hist(window(rstandard(m1.co2),start=c(1995,2)),xlab='Standardized Residuals',
main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

# Exhibit 10.14
win.graph(width=4, height=4,pointsize=8)
qqnorm(window(rstandard(m1.co2),start=c(1995,2)),main=expression(Normal~~Q-Q~~Plot))
title(main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model),
line=3)
qqline(window(rstandard(m1.co2),start=c(1995,2)))

# Exhibit 10.15
m2.co2=arima(co2,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))
m2.co2

# Exhibit 10.16
win.graph(width=4.875, height=3,pointsize=8)
plot(m1.co2,n1=c(2003,1),n.ahead=24,col='red',xlab='Year',type='o',
ylab=expression(CO[2]~~Levels),
main=expression(Forecasts~~and~~Forecast~~Limits~~'for'~~the~~CO[2]~~Model))
# Note that for is a reserved word in R so it has to be enclosed in quotation marks.

# Exhibit 10.17
plot(m1.co2,n1=c(2004,1),n.ahead=48,col='red',xlab='Year',type='o',
ylab=expression(CO[2]~~Levels),
main=expression(Long~~Term~~Forecasts~~'for'~~the~~CO[2]~~Model))

