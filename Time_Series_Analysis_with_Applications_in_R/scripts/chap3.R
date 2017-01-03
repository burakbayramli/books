# Exhibit 3.1
# time(rwalk) yields a time series of the time epoches when the random walk was sampled.
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Exhibit 3.2
win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
plot(rwalk,type='o',ylab='y')
abline(model1) # add the fitted least squares line

# Exhibit 3.3
# season(tempdub) creates a vector of the month index of the data as a factor 
data(tempdub)
month.=season(tempdub) # the period sign is included to make the printout from
# the commands two line below clearer; ditto below.
model2=lm(tempdub~month.-1) # -1 removes the intercept term 
summary(model2)

# Exhibit 3.4
model3=lm(tempdub~month.) # intercept is automatically included so one month (Jan) is dropped
summary(model3)

# Exhibit 3.5
# first creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)

# Exhibit 3.6
win.graph(width=4.875, height=2.5,pointsize=8)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
ylim=range(c(fitted(model4),tempdub))) # the ylim option ensures that the 
# y axis has a range that fits the raw data and the fitted values
points(tempdub)

# Exhibit 3.7
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Exhibit 3.8
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
ylab='Standardized Residuals',type='o')

# Exhibit 3.9
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
ylab='Standardized Residuals',type='l')
points(y=rstudent(model3),x=as.vector(time(tempdub)),
pch=as.vector(season(tempdub)))

# Exhibit 3.10
plot(y=rstudent(model3),x=as.vector(fitted(model3)),xlab='Fitted Trend Values',
ylab='Standardized Residuals',type="n")
points(y=rstudent(model3),x=as.vector(fitted(model3)),
pch=as.vector(season(tempdub)))

# Exhibit 3.11
hist(rstudent(model3),xlab='Standardized Residuals',main='')

# Exhibit 3.12
win.graph(width=3, height=3,pointsize=8)
qqnorm(rstudent(model3),main='')

# Exhibit 3.13
win.graph(width=4.875, height=3,pointsize=8)
acf(rstudent(model3),main='')

# Exhibit 3.14
plot(y=rstudent(model1),x=as.vector(time(rwalk)),ylab='Standardized Residuals',
xlab='Time',type='o')

# Exhibit 3.15
win.graph(width=4.875, height=3,pointsize=8)
plot(y=rstudent(model1),x=fitted(model1),ylab='Standardized Residuals',
xlab='Fitted Trend Values',type='p')

# Exhibit 3.16
acf(rstudent(model1),main='')






