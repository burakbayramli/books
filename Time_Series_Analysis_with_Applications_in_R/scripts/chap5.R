# Chap5.R

# Exhibit 5.1
win.graph(width=4.875, height=3,pointsize=8)
data(oil.price)
plot(oil.price, ylab='Price per Barrel',type='l')

# Exhibit 5.3
data(explode.s)
plot(explode.s,ylab=expression(y[t]),type='o')

# Exhibit 5.4
plot(diff(log(oil.price)),ylab='Change in Log(Price)',type='l')

# Exhibit 5.5
data(ima22.s)
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')

# Exhibit 5.6
plot(diff(ima22.s),ylab='First Difference',type='o')

# Exhibit 5.7
plot(diff(ima22.s,difference=2),ylab='Differenced Twice',type='o')

# Note that plot(diff(ima22.s,2),ylab='Differenced Twice',type='o') will
# draw a wrong figure because the second argument is the lag not the times of
# differencing. That is, diff(ima22.s,2) is the series of ima22.s(t)-ima22.s(t-2).
 

# Exhibit 5.8
data(electricity)
plot(electricity)

# Exhibit 5.9
plot(log(electricity),ylab='Log(electricity)') # without specifying the y-label
# the default y-label is "electricity" rather than "log(electricity)"!

# Exhibit 5.10
plot(diff(log(electricity)),ylab='Difference of Log(electricity)')

# Exhibit 5.11
win.graph(width=3, height=3,pointsize=8)
BoxCox.ar(electricity) 
# In case the function fails, check wheher all data are positive. If
# not, shift all data by a fixed constant. If the function still fails,
# try setting method='ols', as an alternative
# to the default method of 'mle'.
