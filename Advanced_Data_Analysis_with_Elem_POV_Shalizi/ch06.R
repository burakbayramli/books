#############################################################
# R code to accompany notes for bootstrapping               #
# 36-402, Spring 2012                                       #
#############################################################

##### Code Example 1 #####
# Sketches of functions for performing basic bootstrap inference tasks
# These are not very flexible and should probably be thought of as pseudo-code
# or sketches for more complicated industrial-strength functions
  # In particular, consider using the "boot" package from CRAN for real-world
  # projetcts

# Perform multiple independent simulations and calculate the same statistic
# on each
# Inputs: Number of bootstrap replicates (B)
  # Function to take a data set and return an estimate (statistic)
  # Function to produce a simulated data set (simulator)
# Presumes: Output of simulator is formatted to be input for statistic
  # statistic needs no extra arguments
# Output: Array of bootstrapped statistic values
rboot <- function(B, statistic, simulator) {
  tboots <- replicate(B, statistic(simulator()))
  return(tboots)
}

# Calculate bootstrap standard errors in a statistic
# Inputs: Function to produce a simulated data set (simulator)
  # Function to calculate the statistic (statistic)
  # Number of bootstrap replicates (B)
# Calls: rboot
# Presumes: Output of simulator is formatted to be input for statistic
  # statistic needs no extra arguments
  # statistic() returns a numeric scalar
# Output: the standard error
bootstrap.se <- function(simulator, statistic, B) {
  tboots <- rboot(B, statistic, simulator)
  se <- sd(tboots)
  return(se)
}

##### Code Example 2 #####
# See comments at the head of Code Example 1 

# Bootstrap bias calculation
# Inputs: function to make a simulated data set (simulator)
  # Function to calculate the statistic (statistic)
  # Number of bootstrap replicates (B)
  # Empirical value of statistic (t.hat)
# Calls: rboot
# Presumes:  Output of simulator is formatted to be input for statistic
  # statistic needs no extra arguments
  # The mean function works on statistic's return values
# Output: The bias
bootstrap.bias <- function(simulator, statistic, B, t.hat) {
  tboots <- rboot(B, statistic, simulator)
  bias <- mean(tboots) - t.hat
  return(bias)
}



##### Code Example 3 ######
# See comments at the head of Code Example 1

# Bootstrap confidence intervals
  # Applies the basic or "pivot" method, see notes
# Inputs: function to make a simulated data set (simulator)
  # Function to calculate the statistic (statistic)
  # Number of bootstrap replicates (B)
  # Empirical value of statistic (t.hat)
  # One minus the desired confidence level (alpha)
# Calls: rboot
# Presumes:  Output of simulator is formatted to be input for statistic
  # statistic needs no extra arguments
  # The quantile function works on statistic's return values
  # alpha is in [0,1]
# Output: List with the lower and upper confidence limits (ci.lower, ci.upper)
bootstrap.ci.basic <- function(simulator, statistic, B, t.hat,
                               alpha) {
  tboots <- rboot(B,statistic, simulator)
  ci.lower <- 2*t.hat - quantile(tboots,1-alpha/2)
  ci.upper <- 2*t.hat - quantile(tboots,alpha/2)
  return(list(ci.lower=ci.lower,ci.upper=ci.upper))
}

##### Code Example 4 #####
# See comments at the head of Code Example 1

# Calculate p-value by bootstrapping
# Inputs: function to calculate a test statistic (test)
  # Function to produce a surrogate data set (simulator)
  # Number of bootstrap replicates (B)
  # Observed value of test statistic (testhat)
# Calls: rboot
# Presumes:  Output of simulator is formatted to be input for test
  # test needs no extra arguments
  # test returns a value for which the comparison >= is defined
# Output: The p-value
boot.pvalue <- function(test,simulator,B,testhat) {
  testboot <- rboot(B=B, statistic=test, simulator=simulator)
  p <- (sum(test >= testhat)+1)/(B+1)
  return(p)
}


##### Code Example 5 #####
# See comments at the head of Code Example 1

# Calculate p-values by the "double bootstrap", correcting for the effects of
# parameter estimation

# Inputs: function to calculate a test statistic (test)
  # Function to produce a surrogate data set (simulator)
  # Number of first-level bootstrap replicates (B1)
  # Number of second-level bootstrap replicates (B2)
  # Function to estimate parameter from data (estimator)
  # Empirical estimate of parameter (thetahat)
  # Observed value of test statistic (testhat)
# Calls: rboot, boot.pvalue
# Presumes:  Output of simulator is formatted to be input for test
  # test needs no extra arguments
  # test returns a value for which the comparison >= is defined
  # simulator takes an argument named theta
  # The output of estimator has the right format to be the theta argument
# Output: The p-value
doubleboot.pvalue <- function(test,simulator,B1,B2, estimator, 
                            thetahat, testhat) {
  for (i in 1:B1) {
    xboot <- simulator(theta=thetahat)
    thetaboot <- estimator(xboot)
    testboot[i] <- test(xboot)
    pboot[i] <- boot.pvalue(test,simulator,B2,testhat=testboot[i],
      theta=thetaboot)
  }
  p <- (sum(testboot >= testhat)+1)/(B1+1)
  p.adj <- (sum(pboot <= p)+1)/(B1+1)
}
   # R exercise for the reader: replace the inner for() loop with something
   # more vectorized


###### Set up the Pareto law of wealth example
  # Data file and pareto.R are both available through the class website
# Load the Pareto-related functions
source("http://www.stat.cmu.edu/~cshalizi/uADA/12/pareto.R")
# Load the data
wealth <- scan("http://www.stat.cmu.edu/~cshalizi/uADA/12/wealth.dat")
# Fit the distribution
wealth.pareto <- pareto.fit(wealth,threshold=9e8)
  # pareto.fit() will go through a somewhat complicated procedure to estimate
  # the scaling threshold, if run with threshold="find" (the default value).
  # (See Clauset et al. 2009.)  That's where this threshold came from.

##### Figure 2 #####
  # See caption in notes
  # plot.survival.loglog is from pareto.R

plot.survival.loglog(wealth,xlab="Net worth (dollars)",
  ylab="Fraction of individuals at or above that net worth")
rug(wealth,side=1,col="grey")
curve((302/400)*ppareto(x,threshold=9e8,exponent=2.34,lower.tail=FALSE),
  add=TRUE,lty=2,from=9e8,to=2*max(wealth))


##### Code Example 6 #####
# Parametric bootstrap standard error and bias for the Pareto

# Generate multiple data sets from the Pareto and re-estimate the exponent on
  # each one
# Inputs: Number of bootstrap replicates (B)
  # Scaling exponent (exponent)
  # Threshold (x0)
  # Size of each data set (n)
# Calls: rpareto and pareto.fit from pareto.R
# Output: Vector of B estimates of the exponent
rboot.pareto <- function(B,exponent,x0,n) {
  replicate(B,pareto.fit(rpareto(n,x0,exponent),x0)$exponent)
}

# Parametric boostrap standard error for the Pareto exponent
# Inputs: Number of bootstrap replicates (B)
  # Scaling exponent (exponent)
  # Threshold (x0)
  # Size of each data set (n)
# Calls: rboot.pareto
# Output: The standard error
pareto.se <- function(B,exponent,x0,n) {
  return(sd(rboot.pareto(B,exponent,x0,n)))
}

# Parametric boostrap bias for the Pareto exponent
# Inputs: Number of bootstrap replicates (B)
  # Scaling exponent (exponent)
  # Threshold (x0)
  # Size of each data set (n)
# Calls: rboot.pareto
# Output: The bias
pareto.bias <- function(B,exponent,x0,n) {
  return(mean(rboot.pareto(B,exponent,x0,n)) - exponent)
}

##### Code Example 7 #####
# Parametric bootstrap confidence interval for the Pareto exponent
  # Uses the basic pivotal method

# Inputs: Number of bootstrap replicates (B)
  # Scaling exponent (exponent)
  # Threshold (x0)
  # Size of each data set (n)
# Calls: rboot.pareto
# Output: list with lower and upper confidence limits (ci.lower,ci.upper)
pareto.ci <- function(B,exponent,x0,n,alpha) {
  tboot <- rboot.pareto(B,exponent,x0,n)
  ci.lower <- 2*exponent - quantile(tboot,1-alpha/2)
  ci.upper <- 2*exponent - quantile(tboot,alpha/2)
  return(list(ci.lower=ci.lower, ci.upper=ci.upper))
}


# Code Example 8
# Bootstrapped Kolmogorov-Smirnov test for the Pareto distribution
  # Use the bootstrap to find a valid p-value for the KS test, correcting
  # for estimating the exponent

# Calculate the KS test statistic on the tail of the data
# Input: Data set (data)
  # Estimated scaling exponent (exponet)
  # Threshold (x0)
# Calls: ppareto from pareto.R
# Presumes: data is a vector of numerical values
  # exponent is a single number (>= 1)
  # x0 is a positive number
  # At least one value in data >= x0
# Output: the KS statistic
ks.stat.pareto <- function(data, exponent, x0) {
  ks.test(data[data>=x0], ppareto, exponent=exponent, threshold=x0)$statistic
}

# Calculate p-values in the bootstrapped KS test
# Inputs: Number of bootstrapped replicates (B)
  # Data set (data)
  # Estimated scaling exponent (exponet)
  # Threshold (x0)
# Calls: ks.stat.pareto from above, rpareto and pareto.fit from pareto.R
# Presumes: B is a positive integer
  # data is a vector of numerical values
  # exponent is a single number (>= 1)
  # x0 is a positive number
  # At least one value in data >= x0
# Output: The p-value
ks.pvalue.pareto <- function(B, data, exponent, x0) {
  testhat <- ks.stat.pareto(data, exponent, x0)
  testboot <- vector(length=B)
  for (i in 1:B) {
    xboot <- rpareto(length(data),exponent=exponent,threshold=x0)
    exp.boot <- pareto.fit(xboot,threshold=x0)$exponent
    testboot[i] <- ks.stat.pareto(xboot,exp.boot,x0)
  }
  p <- (sum(testboot >= testhat)+1)/(B+1)
  return(p)
}



##### Code Example 9 #####
# Nonparametric bootstrap confidence intervals for the Pareto exponent

# Utility function for resampling from a vector
  # Resamples its argument with replacement
# Inputs: Vector to resample from (x)
# Presumes: x is a vector which sample() can handle
# Output: A vector of the same length as x
resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}

# Resample data and fit a Pareto distribution to it multiple times
# Inputs: Number of bootstrap replicates (B)
  # Data vector to resample (data)
  # Threshold for the Pareto distribution (x0)
# Calls: pareto.fit in pareto.R, resample from above
# Presumes: data is a vector of positive numbers
  # x0 is a positive scalar
  # At least some values in data are >= x0
  # B is a positive integer
# Output: Vector of estimated scaling exponents
resamp.pareto <- function(B,data,x0) {
  replicate(B,pareto.fit(resample(data),threshold=x0)$exponent)
}

# Pareto exponent confidence intervals from resampling
  # Basic pivotal method
# Inputs: Number of bootstrap replicates (B)
  # Data vector to resample (data)
  # One minus confidence level (alpha)
  # Threshold for the Pareto distribution (x0)
# Calls: resamp.pareto above
# Presumes: data is a vector of positive numbers
  # x0 is a positive scalar
  # At least some values in data are >= x0
  # B is a positive integer
  # alpha is in [0,1]
# Output: List with lower and upper confidence limits (ci.lower,ci.upper)
resamp.pareto.CI <- function(B,data,alpha,x0) {
  thetahat <- pareto.fit(data,threshold=x0)$exponent
  thetaboot <- resamp.pareto(B,data,x0)
  ci.lower <- thetahat - (quantile(thetaboot,1-alpha/2) - thetahat)
  ci.upper <- thetahat - (quantile(thetaboot,alpha/2) - thetahat)
  return(list(ci.lower=ci.lower,ci.upper=ci.upper))
}



#### Set up example for section 4.1 ####
library(MASS)
data(geyser)
geyser.lm <- lm(waiting~duration,data=geyser)

# Resample rows from the geyser data
  # Notice that it's sufficient to resample row numbers from the data frame,
  # then take those rows
# Inputs: None
# Calls: resample (above)
# Presumes: geyser exists and has rows and columns
# Output: Vector of resampled row indices
resample.geyser <- function() {
  sample.rows <- resample(1:nrow(geyser))
  return(sample.rows)
}

# Linearly regress waiting on duration and return the coefficient vector
# Inputs: Vector of row indices (subset), data frame on which to estimate (data)
# Presumes: data is a data frame
  # data has columns named waiting and duration, containing numerical values
# Output: Vector of estimated intercept and slope
est.waiting.on.duration <- function(subset,data=geyser) {
  fit <- lm(waiting ~ duration, data=data,subset=subset)
  return(coefficients(fit))
}

#### Code Example 10 ####
# Nonparametric bootstrapped confidence intervals for the linear model of
# Old Faithful, by resampling data points
  # Basic pivot method
# Inputs: Number of bootstrap replicates (B)
  # One minus the desired confidence level (alpha)
# Calls: resample.geyser, est.waiting.on.duration
# Presumes: geyser.lm exists, has suitable coefficients
  # B is a positive integer
  # alpha is in [0,1]
# Output: 2x2 array of upper and lower limits for intercept and slope
geyser.lm.cis <- function(B,alpha) {
  tboot <- replicate(B,est.waiting.on.duration(resample.geyser()))
  low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
  high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
  low.cis <- 2*coefficients(geyser.lm) - high.quantiles
  high.cis <- 2*coefficients(geyser.lm) - low.quantiles
  cis <- rbind(low.cis,high.cis)
  rownames(cis) <- as.character(c(alpha/2,1-alpha/2))
  return(cis)
}


#### Set up example for section 4.2 ####
library(np)

# Kernel regression of waiting time on duration
  # The "tol" and "ftol" arguments control how aggressively npreg() tries
  # to optimize the bandwidth (it keeps going until the objective function
  # changes by no more than tol and the gradient is no more than ftol).
  # Playing around ith the whole data, setting these to ~0.1 here speeds things
  # up by several orders of magnitude, without appreciably changing the
  # estimated curve
# Inputs: Vector of row indices (subset), data frame to estimate in (data),
  # "tol" and "ftol" settings for bandwidth selection
# Calls: npreg from np
# Presumes: data is a data frame
  # data has columns of numerical values called waiting and duration
# Output: The fitted npregression object
npr.waiting.on.duration <- function(subset,data=geyser,tol=0.1,ftol=0.1) {
  bw <- npregbw(waiting ~ duration, data=data, subset=subset,
    tol=tol, ftol=ftol)
  fit <- npreg(bw)
  # The natural thing to do would be to say
  ### fit <- npreg(waiting~duration,data=data,subset=subset)
  # but for obscure reasons this does not work when we pass in data as an
  # argument (ask for details if you really want to know)
  # Instead we first fit the bandwidth object with npregbw(), and then use
  # that bandwidth to create the kernel regression with npreg()
  return(fit)
}

# Fit a baseline model to all the data
geyser.npr <- npr.waiting.on.duration(1:nrow(geyser))

##### Code Example 11 #####
# Calculate pointwise confidence bands for kernel regression of Old Faithful
# by resampling data points
  # Because we resample data points, the training set will be different for
  # each bootstrap replicate.  Thus fitted() values will not be comparable
  # across replicates.  Instead, define a grid of points, evenly spaced along
  # the duration axis, and evaluate each kernel regression model on these
  # points.

# Extends just slightly beyond the data
evaluation.points <- seq(from=0.8,to=5.5,length.out=200)
# Make this a data frame with one column, named "duration", so that the
  # predict() function is happy
evaluation.points <- data.frame(duration=evaluation.points)

# Wrapper for evaluating a kernel regression on the grid
# Input: regression model object (npr)
# Presumes: npr has a predct() method
  # evaluation.points exists, and contain columns with the same names as the
  # predictor variables used to fit npr
# Returns: The predictions
eval.npr <- function(npr) {
  return(predict(npr,newdata=evaluation.points))
}

# Get the predictions on the grid-points for our first model
main.curve <- eval.npr(geyser.npr)

# Pointwise kernel regression confidence bands by resampling data-points
  # Applies the basic pivotal method to the predictions at each point on the
  # evaluation grid
# Inputs: Number of bootstrap replicates (B)
  # One minus confidence level (alpha)
# Calls: eval.npr, npr.waiting.on.duration, resample.geyser
# Output: list containing two arrays
  # cis has the lower and upper confidence limits for each evaluation point
  # tboot has all B curves at the evaluation points

npr.cis <- function(B,alpha) {
  tboot <- replicate(B,eval.npr(npr.waiting.on.duration(resample.geyser())))
  low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
  high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
  low.cis <- 2*main.curve - high.quantiles
  high.cis <- 2*main.curve - low.quantiles
  cis <- rbind(low.cis,high.cis)
  # Currently the evaluation points correspond to columns of cis but to the
  # rows of tboot; it'd be nicer to have them oriented the same way, so
  # transpose one of them
  return(list(cis=cis,tboot=t(tboot)))
}


##### Figure 4 #####
# Plot confidence bands for the kernel regression model of Old Faithful

# The next line takes 4 minutes to run on my laptop; it's needed for the
# rest of the plot, but only run it when you are ready!
##### geyser.npr.cis <- npr.cis(B=800,alpha=0.05)
# Set up plotting window, but don't plot anything.
plot(0,type="n",xlim=c(0.8,5.5),ylim=c(0,100),
     xlab="Duration (min)", ylab="Waiting (min)")
# Add thin grey lines for the resampled curves
for (i in 1:800) {
  lines(evaluation.points,geyser.npr.cis$tboot[i,],lwd=0.1,col="grey")
}
# Lower confidence limit
lines(evaluation.points,geyser.npr.cis$cis[1,])
# Upper confidence limit
lines(evaluation.points,geyser.npr.cis$cis[2,])
# Initial estimate on full data
lines(evaluation.points,main.curve)
# Tick marks on x axis for where the data were
rug(geyser$duration,side=1)
# Scatterplot of actual values
points(geyser$duration,geyser$waiting)


##### Set up example for section 4.3 #####
penn <- read.csv("http://www.stat.cmu.edu/~cshalizi/uADA/12/hw/02/penn-select.csv")
penn.formula <- "gdp.growth ~ log(gdp) + pop.growth + invest + trade"
penn.lm <- lm(penn.formula, data=penn)



##### Code Example 12 #####
# Confidence intervals for multiple linear regression by resampling residuals

# Resample residuals from the linear model for the Penn world tables data
# Inputs: none
# Calls: resample
# Presumes: penn exists, is a data frame
  # penn.lm exists, has fitted() and residuals() method
# Output: Data frame with old input variables and new growth values
resample.residuals.penn <- function() {
  # Resampling residuals leaves the independent variables alone, so copy them
  new.frame <- penn
  # Take the old fitted values, and perturb them by resampling the residuals
  new.growths <- fitted(penn.lm) + resample(residuals(penn.lm))
  # Make these the new values of the response
  new.frame$gdp.growth <- new.growths
  # We're done
  return(new.frame)
}

# Wrapper for estimating the Penn world tables linear model on a data frame
# Inputs: Data frame (data)
# Presumes: data has appropriate columns
  # penn.formula exists and is a valid linear regression formula
# Returns: Vector of linear regression coefficients
penn.estimator <- function(data) {
  fit <- lm(penn.formula, data=data)
  return(coefficients(fit))
}

# Confidence intervals by resampling residuals
  # Basic pivotal method
# Input: Number of bootstrap replicates (B)
  # One minus desired confidence level(alpha)
# Calls: penn.estimator, resample.residuals.penn
# Presumes: B is a positive integer
  # alpha is in [0,1]
# Output: array of upper and lower confidence limits for each coefficient
penn.lm.cis <- function(B,alpha) {
  tboot <- replicate(B,penn.estimator(resample.residuals.penn()))
  low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
  high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
  low.cis <- 2*coefficients(penn.lm) - high.quantiles
  high.cis <- 2*coefficients(penn.lm) - low.quantiles
  cis <- rbind(low.cis,high.cis)
  return(cis)
}

##### Example for section 6 #####
# Nonparametric bootstrapping does badly on things where changing a single
# data point can drastically change the result, like extremes of distributions
# Here we show that using resampling to get confidence intervals for the
# maximum of a uniform distribution is an EPIC FAIL



# Calculate actual coverage probability of what looks like a 95% CI
  # Presume we know X~Unif(0,theta), and are trying to estimate theta
  # In reality, theta is fixed at 1
  # The MLE is max(x)
  # Draw 1000 bootstrap replicates by resampling x, and take the max on each
  # Find the quantiles of these re-estimates and correspond 95% CI
  # Check if the CI covers 1 (the true value of theta)
# Inputs: None
# Calls: resample
# Outputs: TRUE if the CI covers 1, FALSE otherwise
is.covered <- function() {
  x <- runif(100)
  max.boot <- replicate(1e3,max(resample(x)))
  # all() takes a vector of Boolean quantities and returns TRUE if all are TRUE
  # The any() function similarly returns TRUE if any of its arguments are TRUE
  all(1 >= 2*max(x) - quantile(max.boot,0.975), 
      1 <= 2*max(x) - quantile(max.boot,0.025))
}


sum(replicate(1000,is.covered()))
# I got 19 rather than about 950
