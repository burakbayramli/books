# chap8.R

# Exhibit 8.1

win.graph(width=4.875, height=3,pointsize=8)
data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color
plot(rstandard(m1.color),ylab='Standardized residuals',type='b')
abline(h=0)

# Exhibit 8.2

data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
m1.hare # the AR(2) coefficient is not significant; it is second in the
# list of coefficients.
m2.hare=arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA)) # fixed the AR(2)
# coefficient to be 0 via the fixed argument.
m2.hare
# Note that the intercept term is actually the mean in the centered form
# of the ARMA model, i.e. if y(t)=sqrt(hare)-intercept, then the model is
# y(t)=0.919*y(t-1)-0.5313*y(t-3)+e(t) 
# So the "ture" intercept equals 5.6889*(1-0.919+0.5313)=3.483, as stated in
# the book!
plot(rstandard(m2.hare),ylab='Standardized residuals',type='b')
abline(h=0)

# Exhibit 8.3
data(oil.price)
m1.oil=arima(log(oil.price),order=c(0,1,1))
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)

# Exhibit 8.4
win.graph(width=3, height=3,pointsize=8)
qqnorm(residuals(m1.color))
qqline(residuals(m1.color))

# Exhibit 8.5
qqnorm(residuals(m1.hare))
qqline(residuals(m1.hare))

# Exhibit 8.6
qqnorm(residuals(m1.oil))
qqline(residuals(m1.oil))

# Exhibit 8.9
win.graph(width=4.875, height=3,pointsize=8)
acf(residuals(m1.color),main='Sample ACF of Residuals from AR(1) Model for Color')

# Exhibit 8.10
acf(residuals(arima(sqrt(hare),order=c(2,0,0))),main='Sample ACF of Residuals from AR(2) Model for Hare
')

# Exhibit 8.11
acf(residuals(m1.color),plot=F)$acf
signif(acf(residuals(m1.color),plot=F)$acf[1:6],2)# to display the first 6 acf
# to 2 significant digits.

# Exhibit 8.12 
win.graph(width=4.875, height=4.5)
tsdiag(m1.color,gof=15,omit.initial=F) # the tsdiag function is modified from that in the
# stats package of R.

# Exhibit 8.13
m1.color 

# Exhibit 8.14
m2.color=arima(color,order=c(2,0,0))
m2.color

# Exhibit 8.15
m3.color=arima(color,order=c(1,0,1))
m3.color

# Exhibit 8.16
m4.color=arima(color,order=c(2,0,1))
m4.color
