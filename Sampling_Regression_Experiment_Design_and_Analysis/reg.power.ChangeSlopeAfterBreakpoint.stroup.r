# Simple Linear regression Power function that incorporates process and sampling error
# that look at power to detect change in slope after a KNOWN breakpoint.

# While this is commonly used for trend analysis where the X variable is time, it can
# also be used for any regression problem where the breakpoint is KNOWN.
# Autocorrelation is not accounted for in this power analysis.

# 2014-11-21 CJS Removed reference to Ivalue
# 2014-11-02 CJS First Edition for web

# This function computes the power to detect a change in slope after a KNOWN breakpoint
# for a simple linear regression design that allows
# for process and sampling error.
#
# A typical usage is to see if an event (e.g. remediation, environmental impact) at a known
# time point, changes the rate of trend following the KNOWN breakpoint.
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        Process.SD  - standard deviation of the process variation over the X value
#        Sampling.SD - standard deviation of the sampling variation at each X value
#     Trend.before.breakpoint   - slope before the breakpoint
#     BreakPoint                - KNOWN breakpoint
#     Trend.after.breakpoint    - slope AFTER the breakpoint
#     X       - vector of X values. These can be in any order. Multiple X values
#               indicated multiple measurements at each X value.
#               These values shoud presumably span the breakpoint.


# The computations are based on the 
#    Stroup, W. W. (1999)
#    Mixed model procedures to assess power, precision, and sample size in the design of experiments.
# paper where "dummy" data is generated and "analyzed" and the resulting F-statistics etc
# provide information needed to compute the power

library(lme4)


#-----------------------------------------------

reg.power.ChangeSlopeAfterBreakpoint.stroup <- function( 
          Trend.before.breakpoint, BreakPoint, Trend.after.breakpoint,
          Xvalues, Process.SD, Sampling.SD, alpha=0.05){
# This computes the power in a simple linear regression to detect a change in the slope
# between before and after a KNOWN breakpoint (BP). This routine allows for 
# process and sampling error. Arguments are defined above
  
# The model for a simple linear regression with a KNOWN breakpoint is very simple
# to implement
#      Y ~ X + (X-BP)+
# where BP is the known breakpoint, and (X-BP)+ is 0 if X<BR and (X-BP) if X-BP is positive.
#
# The coefficient associated with (X-BP)+ is the DIFFERENCE in the slope and a test that this
# coefficient is 0 is equivalent to a test of no change in the slope.
  
# There are 3 cases to consider
# (a) Process.SD >0, Sampling.SD >0, replicates at some X values
#      This is the classical regression model with process error and sampling error.
#      Because there are multiple measurements at some X values, it is possible to
#      fit a model of the form
#        lmer( Y ~ X +(X-BP)+  + (1|XF), data=blah)
#      where XF are the X values treated as a factor.
#      The power refers to the ability to detect a non-zero difference in the slope
#      The approximate degrees of freedom for the error term are the number of unique X values - 3.

#
# (b) Process.SD >0, Sampling.SD >0, NO replicates at any X value
#     This is quite common where an estimate of a population parameter is obtained
#     each year with a measure of precision (SE of each estimate). The sampling SD
#     is essentially the average of the SE. Process error is obatined by subtracting
#     the average SE from the residual sd after fitting a mean (or simple slope)
#     In this case, all that need be done is fit a 
#           lm( Y ~ X + (X-BP)+ , data=blah)
#     as then the resiudal sd is the (proper) mixture of process and sampling SD
#     The power refers to the ability to detect a non-zero difference in the slope.
  
#     If you want to investigate the impact of increasing effort in each year, treat
#     the current average se as obtained from a "unit of effort". So if you take two 
#     measurement at an X value, this is (approximately) equivalent to doubling the effort.
  
#     The approximate degrees of freedom for the error term are the number of unique X values - 3.

#
# (c) Process.SD = 0, Sampling.SD >0, any set of X values (with or without replicates
#     at an paticular X values)
#     This is a classical simple linear regression with data points scattered about
#     the regression line and all points are independent of all other points,i.e.
#          lm(Y ~ X + (X-BP)+)
#     This is a VERY strong assumption and typically not valid when testing for 
#     trends over time where it is VERY common to have a process error that corresponds to
#     year specific effects over which you typically do not have any control.
  
#     The approximate degrees of freedom for the error term are the number of  X values - 3.

#
  #browser()
  # Total sample size
  np <- length(Xvalues)
  # The Stroup method first finds the mean response at each X value.
  # Compute the mean response (before adding process o sampling error)
  # 
  mu <- 0 + Trend.before.breakpoint*(Xvalues-min(Xvalues))+
                 (Trend.after.breakpoint-Trend.before.breakpoint)*pmax(0, Xvalues - BreakPoint)
  
  # Create the various design matrices
  # Fixed effects
  X  <- cbind(1, Xvalues, pmax(0, Xvalues-BreakPoint))
  # Random effects (for the process error)
  XF <- model.matrix(~ -1 +as.factor(Xvalues))

  # We solve for the regression estimates using weighted least squares
  # based on the variance-covariance matrix for the data
  V <- diag(Sampling.SD^2,np,np) + XF %*% t(XF)*Process.SD^2 

  # Get fixed effects and fixed effects varcovar matrix
  beta <- solve(t(X)%*%solve(V)%*%X) %*% t(X)%*%solve(V)%*%mu

# the vector to extract the slope coefficient
  K <- c(0,0,1)

#  calculate the non-centrality parameter, and then the power
  ncp <- as.numeric(t(t(K)%*%beta)%*%solve(t(K)%*%solve(t(X)%*%solve(V)%*%X)%*%K)%*%(t(K)%*%beta))

# What is the denominator df for the hypothesis test. See notes above
  dfdenom <- length(unique(Xvalues))-3 # approximation to df for slope is number of unique X values -3 
  if(Process.SD ==0){ dfdenom = length(Xvalues)-3}

  Fcrit <- qf(1-alpha, 1, dfdenom)
  power <- 1 - pf(Fcrit, 1, dfdenom,ncp)

#  Compute the one-sided power, i.e. to detect the change in one direction only
#  Because I don't know which direction of interest, the one-sided power is 
#  computed in both directions.
#
  Tcrit <- qt(1-alpha,dfdenom)
  os.power1 <- 1-pt(Tcrit,dfdenom,sqrt(ncp))
  os.power2 <- pt(-Tcrit,dfdenom,sqrt(ncp))

  return(c(alpha=alpha, 
     Trend.before.breakpoint=Trend.before.breakpoint,
     BreakPoint = BreakPoint,
     Trend.after.breakpoint=Trend.after.breakpoint,
     Process.SD=Process.SD, Sampling.SD=Sampling.SD,
     Beta=beta,
     dfdenom=dfdenom, ncp=ncp, Fcrit=Fcrit, power=power,
     Tcrit=Tcrit, os.power1=os.power1, os.power2=os.power2))
}

