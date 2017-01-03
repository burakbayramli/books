# Before-After Design Power function that incorporates process and sampling error
# Autocorrelation is not accounted for in this power analysis.

# 2014-11-29 CJS First edition

# This function computes the power for a simple before-after sampling design that allows
# for process and sampling error
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        Process.SD  - standard deviation of the process variation over the X value
#        Sampling.SD - standard deviation of the sampling variation at each X value
#     Shift   - shift in the mean for which the power to detect is to be estimated
#     X.before       - vector of X values taken before the impact. These can be in any order. Multiple X values
#               indicated multiple measurements at each X value.
#     X.after - vector of X values taken after impact. Same conventions as X.before
#     Values of X.after and X.before should NOT overlap.

# The computations are based on the 
#    Stroup, W. W. (1999)
#    Mixed model procedures to assess power, precision, and sample size in the design of experiments.
# paper where "dummy" data is generated and "analyzed" and the resulting F-statistics etc
# provide information needed to compute the power



#-----------------------------------------------

before.after.power.stroup <- function(Shift, X.before, X.after, Process.SD, Sampling.SD, alpha=0.05){
# This computes the power of a before/after study that potentially includes 
# process and sampling error. Arguments are defined above

# There are 3 cases to consider
# (a) Process.SD >0, Sampling.SD >0, replicates at some X values
#      This is the before-after study with multiple measurement in some X-values (Year)
#      with process error and sampling error.
#      Because there are multiple measurements at some X values, it is possible to
#      fit a model of the form
#        lmer( Y ~ Before.After + (1|XF), data=blah)
#      where XF are the unique X values treated as a factor.
#      In this case, the df for testing a hypothesis about the before/after effect is
#      approximately equal to the
#          number of unique X values - 2 (essentially, you analze the averages)
#      The power refers to the ability to detect a non-zero shift of the lmer model.
#
# (b) Process.SD >0, Sampling.SD >0, NO replicates at any X value
#     This is quite common where an estimate of a population parameter is obtained
#     each year with a measure of precision (SE of each estimate). The sampling SD
#     is essentially the average of the SE. Process error is obatined by subtracting
#     the average SE from the residual sd after fitting a mean (or simple slope)
#     In this case, all that need be done is fit a 
#           lm( Y ~ Before.After, data=blah)
#     as then the residual sd is the (proper) mixture of process and sampling SD
#     The df for hypothesis tests about the slope is again approximately equal to 
#           number of unique X values - 2
#     If you want to investigate the impact of increasing effort in each year, treat
#     the current average se as obtained from a "unit of effort". So if you take two 
#     measurement at an X value, this is (approximately) equivalent to doubling the effort.
#
# (c) Process.SD = 0, Sampling.SD >0, any set of X values (with or without replicates
#     at an paticular X values)
#     This is a classical simple linear regression with data points scattered about
#     the regression line and all points are independent of all other points,i.e.
#          lm(Y ~ Before.After)
#     This is a VERY strong assumption and typically not valid when testing for 
#     trends over time where it is VERY common to have a process error that corresponds to
#     year specific effects over which you typically do not have any control.
#     The df for testing hypothese about the slope is number of data points - 2.
  
  require(lme4)
  # Create the design matrix
  Xvalues  <- c(X.before, X.after)
  X  <- cbind(1, Xvalues %in% X.after)  # add the before/after term
  
  # Total sample size
  n <- length(Xvalues)
  # Compute the mean response (before adding process o sampling error)
  mu <- 0 + Shift*(Xvalues %in% X.after )
  
  # Create the various design matrices
  # Fixed effects
  XF <- model.matrix(~ -1 +as.factor(Xvalues))

  # We solve for the anova using  weighted least squares
  # based on the variance-covariance matrix for the data
  V <- diag(Sampling.SD^2,n,n) + XF %*% t(XF)*Process.SD^2 

  # Get fixed effects and fixed effects varcovar matrix
   beta <- solve(t(X)%*%solve(V)%*%X) %*% t(X)%*%solve(V)%*%mu

# the vector to extract the slope coefficient
  K <- c(0,1)

#  calculate the non-centrality parameter, and then the power
  ncp <- as.numeric(t(t(K)%*%beta)%*%solve(t(K)%*%solve(t(X)%*%solve(V)%*%X)%*%K)%*%(t(K)%*%beta))

# What is the denominator df for the hypothesis test. See notes above
  dfdenom <- length(unique(Xvalues))-2 # approximation to df for slope is number of unique X values -2 
  if(Process.SD ==0){ dfdenom = length(Xvalues)-2}

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
     Shift=Shift, 
     Process.SD=Process.SD, Sampling.SD=Sampling.SD,
     Beta=beta,
     dfdenom=dfdenom, ncp=ncp, Fcrit=Fcrit, power=power,
     Tcrit=Tcrit, os.power1=os.power1, os.power2=os.power2))
}

