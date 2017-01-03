# Exhibit 4.2
win.graph(width=4.875, height=3,pointsize=8)
data(ma1.2.s)
plot(ma1.2.s,ylab=expression(Y[t]),type='o')

# An MA(1) series with MA coefficient equal to -0.9 and 
# of length n=100 can be simulated by the following command
set.seed(12345) # initialize the seed of the random number generator so that
# the simulations can be reproduced.
y=arima.sim(model=list(ma=-c(-0.9)),n=100)
# Note that R uses the plus convention in the model formula so the 
# additional minus sign.  

# Exhibit 4.3
win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.2.s,x=zlag(ma1.2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.4
plot(y=ma1.2.s,x=zlag(ma1.2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')


# Exhibit 4.5
win.graph(width=4.875, height=3,pointsize=8)
data(ma1.1.s)
plot(ma1.1.s,ylab=expression(Y[t]),type='o')

# An MA(1) series with ma coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(MA=-c(0.9)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.  


# Exhibit 4.6
win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.1.s,x=zlag(ma1.1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.7
plot(y=ma1.1.s,x=zlag(ma1.1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.8
win.graph(width=4.875, height=3,pointsize=8)
data(ma2.s)
plot(ma2.s,ylab=expression(Y[t]),type='o')

# An MA(2) series with MA coefficients equal to 1 and -0.6 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ma=-c(1, -0.6)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.  

# Exhibit 4.9
win.graph(width=3, height=3,pointsize=8)
plot(y=ma2.s,x=zlag(ma2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.10
plot(y=ma2.s,x=zlag(ma2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.11
plot(y=ma2.s,x=zlag(ma2.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.13
win.graph(width=4.875, height=3,pointsize=8)
data(ar1.s)
plot(ar1.s,ylab=expression(Y[t]),type='o')

# An AR(1) series with AR coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(0.9)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign.  

# Exhibit 4.14
win.graph(width=3, height=3,pointsize=8)
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.15
plot(y=ar1.s,x=zlag(ar1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.16
plot(y=ar1.s,x=zlag(ar1.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.19
win.graph(width=4.875, height=3,pointsize=8)
data(ar2.s)
plot(ar2.s,ylab=expression(Y[t]),type='o')

# An AR(2) series with AR coefficients equal to 1.5 and -0.75 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(1.5,-0.75)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign. 







