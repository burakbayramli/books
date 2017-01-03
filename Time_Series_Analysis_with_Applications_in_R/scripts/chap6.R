# Chap6.R

# Exhibit 6.5
data(ma1.1.s)
win.graph(width=4.875, height=3,pointsize=8)
acf(ma1.1.s,xaxp=c(0,20,10)) # if the arument xaxp is omitted, the
# tick marks will be generated according to the default convention. 
# Run the command ?par to learn more the xaxp argment.  
acf(ma1.1.s) # see the new tickmarks

# So how was xaxp determined? First run the command without xaxp to find out
# how many lags are on the x-axis. Suppose there are 20 lags. Then we specify
# xaxp=c(0,20,10), i.e. the two extreme tickmarks are 0 and 20, and we want
# 10 tickmarks in between. How to set the xaxp argument if we want 1 tickmark for
# each lag?

# Exhibit 6.6
acf(ma1.1.s,ci.type='ma',xaxp=c(0,20,10))

# Exhibit 6.7
data(ma1.2.s)
acf(ma1.2.s,xaxp=c(0,20,10))

# Exhibit 6.8
data(ma2.s)
acf(ma2.s,xaxp=c(0,20,10))

# Exhibit 6.9
acf(ma2.s,ci.type='ma',xaxp=c(0,20,10))

# Exhibit 6.10
data(ar1.s)
acf(ar1.s,xaxp=c(0,20,10))


# Exhibit 6.11
pacf(ar1.s,xaxp=c(0,20,10))

# Exhibit 6.12
data(ar2.s)
acf(ar2.s,xaxp=c(0,20,10))

# Exhibit 6.13
pacf(ar2.s,xaxp=c(0,20,10))

# Exhibit 6.14
data(arma11.s)
plot(arma11.s, type='b',ylab=expression(y[t]))

# Exhibit 6.15
acf(arma11.s,xaxp=c(0,20,10))

# Exhibit 6.16
pacf(arma11.s,xaxp=c(0,20,10))

# Exhibit 6.17
eacf(arma11.s)

# If desirable,  a title can be added to the EACF table by the
# following command.

cat('Exhibit 6.17\n');eacf(arma11.s)

# Exhibit 6.18
data(oil.price)
acf(as.vector(oil.price),main='Sample ACF of the Oil Price Time Series',xaxp=c(0,24,12))
# The as.vector function strips the ts attaribute of oil.price in order to 
# prevent the plotting convention for seasonal time series.
# The main argument supplies the main heading of the figure.
# Try the following command to appreciate the effects of applying the as.vector
# function.

acf(oil.price,main='Sample ACF of the Oil Price Time Series')
# The tickmark 1 on the x-axis now refers to a lag of
# 1 period with each period containing 12 months, so it is lag 12!

# Exhibit 6.19
acf(diff(as.vector(log(oil.price))),main='Sample ACF of the Difference of the Oil Price Time Series',xaxp=c(0,24,12))


# Exhibit 6.20
data(rwalk)
acf(diff(rwalk,difference=2),ci.type='ma',xaxp=c(0,18,9))

# Exhibit 6.21
acf(diff(rwalk),ci.type='ma',xaxp=c(0,18,9))

library(uroot)


# Carry out the Dickey-Fuller unit root tests
# Find out the AR order for the differenced series
ar(diff(rwalk))
# order 8 is indicated by AIC

library(uroot) 
ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8),Pmax=8),itsd=c(1,0,0))
# which is equivalent to
# ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8)),itsd=c(1,0,0))
#
# The argument mode can be either a numerical vector, 
# or "aic","bic" and "signf".
# If it is a numerical vector, the test assumes the vector corresponds to the
# lags of the response that must be included in the test, and the Pmax 
# option is ignored.
# However, if mode is specified as "aic", the function uses the AIC to
# select lags up to and including Pmax to be included in the test.
# Specifying mode to "bic" or "signf" changes the selection criterion
# to BIC or significance criterion. Note "signf" not "signif" is the valid
# option. 


# In comparison, setting the true order to be zero for the first difference
ADF.test(rwalk,selectlags=list(Pmax=0),itsd=c(1,0,0))

# Repeat the test with the alternative have an intercept term and a linear trend
# Run ?uroot to learn more about the selectlags option and the itsd option.

ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8),Pmax=8),itsd=c(1,1,0))
#
# Similarly, a simplified command is 
# ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8)),itsd=c(1,1,0))


# Repeat the preceding test but now setting the true order of the first difference
# as zero.

ADF.test(rwalk,selectlags=list(Pmax=0),itsd=c(1,1,0))


# Exhibit 6.22 
set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),ma=c(rep(0,11),0.7)),n=120)
res=armasubsets(y=test,nar=14,nma=14,y.name='test',ar.method='ols')
plot(res)

# A title may be added to the plot, by adding the main="..." option
# in the above plot function. However, the title may crash with other
# labels. A better approach is to add a title by the 
# following command.

title(main="Exhibit 6.22", line=6)

# The option line=6 means put the title 6 lines from the edge of the
# plot region. You can experiment with this option to fine tune the
# placement of the label.


# Exhibit 6.23
data(larain)
win.graph(width=3, height=3,pointsize=8)
qqnorm(log(larain))
qqline(log(larain))

# Exhibit 6.24
win.graph(width=4.875, height=3,pointsize=8)
acf(log(larain),xaxp=c(0,20,10)) # note the main heading now includes the data name.

# Exhibit 6.25
data(color)
acf(color,ci.type='ma')

# Exhibit 6.26
pacf(color)

# Exhibit 6.27  
win.graph(width=4, height=4,pointsize=8)
data(hare)
bxh=BoxCox.ar(hare)
bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.

# Exhibit 6.28
acf(hare^.5)

# Exhibit 6.29
pacf(hare^.5)

# Exhibit 6.30
eacf(diff(log(oil.price)))

# Exhibit 6.31
res=armasubsets(y=diff(log(oil.price)),nar=7,nma=7,
y.name='test', ar.method='ols')
plot(res)

# Exhibit 6.32
acf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))

# Exhibit 6.33
pacf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))
