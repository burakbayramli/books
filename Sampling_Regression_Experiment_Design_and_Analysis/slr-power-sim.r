# Simple Linear Regression - Power Analysis - Via simulation

# Simple Linear regression Power function that incorporates process and sampling error
# While this is commonly used for trend analysis where the X variable is time, it can
# also be used for any regression problem.
# Autocorrelation is not accounted for in this power analysis.

# 2014-11-21 CJS Removed the Ivalue parameter from the call list
# 2014-06-24 CJS First Edition for web

# This function computes the power for a simple linear regression design that allows
# for process and sampling error
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        Process.SD  - standard deviation of the process variation over the X value
#        Sampling.SD - standard deviation of the sampling variation at each X value
#     Trend   - slope for which the power to detect is to be estimated
#     X       - vector of X values. These can be in any order. Multiple X values
#               indicated multiple measurements at each X value.
#     nsim    - number of replicate simulations used to compute power (default 1000)


#-----------------------------------------------

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(lmerTest)
library(plyr)


#----------------------------------------------------------
# Simulation Study of a regression model with process and non-process error
# See my CourseNotes - the section on Trend Analysis for more details about
# process and sampling error
# Contact cschwarz@stat.sfu.ca for details.


 
slr.power.sim <- function(Trend, Xvalues, Process.SD, Sampling.SD, alpha=0.05, nsim=1000){
# This computes the power of a simple linear regression that potentially includes 
# process and sampling error via simulation Arguments are defined above

# Trend  - size of trend  (change per unit) as an absolute slope
#          An arbitrary intercept of 0 is assumed.
# Xvalues - vector of X values. Often this is time, but not necessarily
#    e.g. c(1,1,2,3,4,5,5) implies two samples taken in year 1,
#          one sample taken in year 2, year 3, year 4, and 2 samples in year 5
#   These values can be in any order. It is not necessary that the sample
#   times start at 1. 
# Sampling.SD - Sampling standard deviation as a absolute number (not a cv)
# Process.SD  - Process standard deviation as an absolute number (not a cv)
# alpha       - alpha level
# nsim        - number of simulations to generate to estimate the power
  
# There are 3 cases to consider
# (a) Process.SD >0, Sampling.SD >0, replicates at some X values
#      This is the classical regression model with process error and sampling error.
#      Because there are multiple measurements at some X values, it is possible to
#      fit a model of the form
#        lmer( Y ~ X + (1|XF), data=blah)
#      where XF are the X values treated as a factor.
#      In this case, the df for testing a hypothesis about the slope is
#      approximately equal to the
#          number of unique X values - 2 (essentially, you analze the averages)
#      The power refers to the ability to detect a non-zero slope of the lmer model.
#
# (b) Process.SD >0, Sampling.SD >0, NO replicates at any X value
#     This is quite common where an estimate of a population parameter is obtained
#     each year with a measure of precision (SE of each estimate). The sampling SD
#     is essentially the average of the SE. Process error is obatined by subtracting
#     the average SE from the residual sd after fitting a mean (or simple slope)
#     In this case, all that need be done is fit a 
#           lm( Y ~ X, data=blah)
#     as then the resiudal sd is the (proper) mixture of process and sampling SD
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
#          lm(Y ~ X)
#     This is a VERY strong assumption and typically not valid when testing for 
#     trends over time where it is VERY common to have a process error that corresponds to
#     year specific effects over which you typically do not have any control.
#     The df for testing hypothese about the slope is number of data points - 2.
 
# Generate data with specified process and sampling error
   GenData <- function(Ivalue=0, Trend, Xvalues, Sampling.SD, Process.SD){
      # Arguments are same as calling function
      mydf <- data.frame(Xvalues=Xvalues)  # the design matrix
      # create a copy of the sample times as a factor for the process error
      mydf$XvaluesF <- as.factor(Xvalues) # return sample times as a factor
      # Compute the mean response (before adding process o sampling error)
      mydf$mu <- Ivalue + Trend*(Xvalues-min(Xvalues))
      # Create a vector of process errors and then add to the relevant rows in the response
      p.errors <- rnorm(length(unique(Xvalues)), mean=0, sd=Process.SD)
      mydf$Y <- mydf$mu + p.errors[match(Xvalues, unique(Xvalues))] 
      # Add sampling error
      mydf$Y <- mydf$Y +  rnorm(length(Xvalues), mean=0, sd=Sampling.SD)
      return(mydf)
   }

# Generate one simulation of the fitting 
# This will generate the data, fit the mixed effect model, 
# and return the coefficient table and p-values 
   do.one.sim <- function(Trend,  Xvalues, Process.SD, Sampling.SD) {
    # arguments defined above
     mydata <- GenData(Ivalue=0,
                Trend=Trend, 
                Xvalues=Xvalues, 
                Process.SD=Process.SD,
                Sampling.SD=Sampling.SD)
  # do the fit. We need to consider the three cases
  if(Process.SD>0 & length(Xvalues)>length(unique(Xvalues))){ # process error with duplicates
     require(lmerTest)
     myfit <- lmer(Y ~ Xvalues + (1| XvaluesF), data=mydata)
     my.coef <- summary(myfit)$coefficients
     my.varcorr <- VarCorr(myfit)
  }
  if(Process.SD==0 | length(Xvalues)==length(unique(Xvalues))){
     myfit <- lm  (Y ~ Xvalues, data=mydata)
     my.coef <- summary(myfit)$coefficients
     my.varcorr <- summary(myfit)$sigma
  }
    
  res <- list(data=mydata, coef=my.coef, VarCorr=my.varcorr)
  return(res)
  }

# Do the simlation a number of times
# We get back a list with the information from each simulation
   myres <- rlply(nsim, do.one.sim(
                Trend=Trend, 
                Xvalues=Xvalues,
                Process.SD = Process.SD,
                Sampling.SD = Sampling.SD))
   # get the power
   slope.pvalue <- ldply(myres, function(x){as.vector(x$coef["Xvalues","Pr(>|t|)"])}) 
   power <- mean(slope.pvalue <= alpha) 
   names(power) <- 'Power.slope'
   return(power)
}
