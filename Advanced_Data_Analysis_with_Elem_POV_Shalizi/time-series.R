# Examples for the lecture on time series

# Real-world data set
library(datasets)
data(lynx)

# Synthetic data set
  # Logistic map corrupted with observational noise
    # Exercise fo the student: why is this "logistic"?
logistic.map <- function(x,r=4) { r*x*(1-x) }
logistic.iteration <- function(n,x.init,r=4){
  x <- vector(length=n)
  x[1] <- x.init
  for (i in 1:(n-1)) {
    x[i+1] <- logistic.map(x[i],r=r)
  }
  return(x)
}
x <- logistic.iteration(1000,x.init=runif(1))
y <- x+rnorm(1000,mean=0,sd=0.05)

# plot all of the lynx series
plot(lynx)
# Plot the first part of the synthetic time series
plot(y[1:100],xlab="t",ylab=expression(y[t]),type="l")

# autocorrelation functions
acf(lynx)
acf(y)

# Convert a time series into a data frame of lagged values
# Input: a time series, maximum lag to use, whether older values go on the right
  # or the left
# Output: a data frame with (order+1) columns, named lag0, lag1, ... , and
  # length(ts)-order rows
design.matrix.from.ts <- function(ts,order,right.older=TRUE) {
  n <- length(ts)
  x <- ts[(order+1):n]
  for (lag in 1:order) {
    if (right.older) {
      x <- cbind(x,ts[(order+1-lag):(n-lag)])
    } else {
      x <- cbind(ts[(order+1-lag):(n-lag)],x)
    }
  }
  lag.names <- c("lag0",paste("lag",1:order,sep=""))
  if (right.older) {
    colnames(x) <- lag.names
  } else {
    colnames(x) <- rev(lag.names)
  }
  return(as.data.frame(x))
}

# Plot successive values of lynx against each other
plot(lag0 ~ lag1,data=design.matrix.from.ts(lynx,1),xlab=expression(lynx[t]),
  ylab=expression(lynx[t+1]),pch=16)
# Plot successive values of y against each other
plot(lag0 ~ lag1,data=design.matrix.from.ts(y,1),xlab=expression(y[t]),
  ylab=expression(y[t+1]),pch=16)

  # Exercise for student: write one function to do either of those plots


# Fit an additive autoregressive model
  # additive model fitting is outsourced to mgcv::gam, with splines
# Inputs: time series (x), order of autoregression (order)
# Output: fitted GAM object
aar <- function(ts,order) {
  stopifnot(require(mgcv))
  # Automatically generate a suitable data frame from the time series
  # and a formula to go along with it
  fit <- gam(as.formula(auto.formula(order)),
    data=design.matrix.from.ts(ts,order))
  return(fit)
}

# Generate formula for an autoregressive GAM of a specified order
# Input: order (integer)
# Output: a formula which looks like
  # "lag0 ~ s(lag1) + s(lag2) + ... + s(lagorder)"
auto.formula <- function(order) {
  inputs <- paste("s(lag",1:order,")",sep="",collapse="+")
    form <- paste("lag0 ~ ",inputs)
    return(form)
}


# Plot successive values of y against each other
plot(lag0 ~ lag1,data=design.matrix.from.ts(y,1),xlab=expression(y[t]),
  ylab=expression(y[t+1]),pch=16)
# Add the linear regression (which would be the AR(1) model)
abline(lm(lag0~lag1,data=design.matrix.from.ts(y,1)),col="red")
# Fit an AR(8) and add its fitted values
yar8 <- arma(y,order=c(8,0))
points(y,fitted(yar8),col="red")
# Fit a first-order nonparametric autoregression, add fitted values
yaar1 <- aar(y,order=1)
points(y[-length(y)],fitted(yaar1),col="blue")


# simple block bootstrap
# inputs: time series (ts), block length, length of output
# presumes: ts is a univariate time series
# output: one resampled time series
rblockboot <- function(ts,block.length,len.out=length(ts)) {
  # chop up ts into blocks
  the.blocks <- as.matrix(design.matrix.from.ts(ts,block.length-1,
    right.older=FALSE))
    # look carefully at design.matrix.from.ts to see why we need the -1
  # How many blocks is that?
  blocks.in.ts <- nrow(the.blocks)
  # Sanity-check
  stopifnot(blocks.in.ts == length(ts) - block.length+1)
  # How many blocks will we need (round up)?
  blocks.needed <- ceiling(len.out/block.length)
  # Sample blocks with replacement
  picked.blocks <- sample(1:blocks.in.ts,size=blocks.needed,replace=TRUE)
  # put the blocks in the randomly-selected order
  x <- the.blocks[picked.blocks,]
  # convert from a matrix to a vector and return
    # need transpose because R goes from vectors to matrices and back column by
    # column, not row by row
  x.vec <- as.vector(t(x))
    # Discard uneeded extra observations at the end silently
  return(x[1:len.out])
}

###############################################################################
# Exercises for the reader:
# 1. Modify this to do the circular block bootstrap (hint: extend ts)
# 2. Modify this to do block bootstrapping of multiple time series, using the
# # SAME blocks for all series (to preserve dependencies across series)
##############################################################################


# GDP per capita time series
  # Taken from the St. Louis Federal Reserve Bank's FRED data service
  # GDP in billions of constant (2005) dollars
  # http://research.stlouisfed.org/fred2/series/GDPCA?cid=106
  # over population in thousands
  # http://research.stlouisfed.org/fred2/series/POP?cid=104
  # so the defualt units are millions of dollars per capita, which is needlessly
  # hard to interpret
gdppc <- read.csv("gdp-pc.csv")
gdppc$y <- gdppc$y*1e6
plot(gdppc,log="y",type="l")


