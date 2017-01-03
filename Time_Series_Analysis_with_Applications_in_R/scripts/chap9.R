# chap9.R

library(TSA)

# Exhibit 9.1
data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color 

# Exhibit 9.2
# append 2 years of missing values to the tempdub data as we want to forecast
# the temperature for two years.
data(tempdub)

tempdub1=ts(c(tempdub,rep(NA,24)),start=start(tempdub),freq=frequency(tempdub)) 

# creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,1)
m5.tempdub=arima(tempdub,order=c(0,0,0),xreg=har.)
m5.tempdub
# The result is same as that from the fit using lm function.
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)


# create the harmonic functions over the period of forecast.
newhar.=harmonic(ts(rep(1,24), start=c(1976,1),freq=12),1)
# Compute and plot the forecasts.
win.graph(width=4.875, height=3,pointsize=8)
plot(m5.tempdub,n.ahead=24,n1=c(1972,1),newxreg=newhar.,
 type='b',ylab='Temperature',xlab='Year')

# Exhibit 9.3 
data(color)
m1.color=arima(color,order=c(1,0,0))
plot(m1.color,n.ahead=12,type='b', xlab='Time', ylab='Color Property')
# add the horizontal line at the estimated mean ("intercept") 
abline(h=coef(m1.color)[names(coef(m1.color))=='intercept'])


# Exhibit 9.4
data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
plot(m1.hare, n.ahead=25,type='b',xlab='Year',ylab='Sqrt(hare)')
abline(h=coef(m1.hare)[names(coef(m1.hare))=='intercept'])

