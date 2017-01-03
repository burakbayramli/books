
# Example of power analysis for simple linear regression on the LOG scale.
# We do this on the log-scale to try and match what happens with Program MONITOR (see my notes)
# 2014-11-21 CJS Removed reference to Ivalue in power analysis
# 2014-07-26 CJS First edition

# Suppose we wish to investigate the power of a monitoring design that will
# run for 5 years. At each survey occasion (i.e. every year), we have 1 monitoring
# station, and we make 2 estimates of the population size at the monitoring station in
# each year. The population is expected to start with 1000 animals, and we expect
# that the measurement error (standard error) in each estimate is about 200, i.e. the coefficient
# of variation of each measurement is about 20\% and is constant over time. We
# are interested in detecting increasing or decreasing trends and to start, a 5\%
# decline per year will be of interest. We will assume an UNREALISTIC process error of zero
# so that the sampling error is equal to the total variation in measurements over time.
#
# In this program, we assume that there is NO process error and that all of the variotion 
# sampling error. IN many trend examples, this won't be true SO READ THE CAUTIONS expressed
# in my notes in the TREND chapter.

# There are two methods for determining power - based on methods by Stroup (1999) or
# via simulation. Both are illustrated here.

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(width=200)            # wide output lines

library(ggplot2)


set.seed(12343124) # only needed if the simulaton method is used.

source("../../Power/RegPower/SLR-power/slr-power-stroup.r") # load the power function
source("../../Power/RegPower/SLR-power/slr-power-sim.r")     # load the power function that does simulation


#----------------------------------------------------------------
# The power functions allow you specify both process and sampling error.
# For the fertilizer experiment, process error is assumed to be zero because
# we assume that each plot's growth is independent of any other plot and
# that all the plots are grown in the same growing season.

# We will do it using both the Stroup method.

# Case 1
options(width=60)
sink("monitor-example1-R-010.txt", split=TRUE)
##***part010b;
Xvalues <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4) # two measurements/years x 5 years
Sampling.SD <- .20 # 20% cv at each year
Trend       <- -.05 # 5% decline/year

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=Trend, Xvalues=Xvalues, 
                 Process.SD=0, Sampling.SD=Sampling.SD, alpha=0.05)
##***part010e;
sink()


# Computation of values for use with Lenth's program
# The key thing to keep in mind is that Lenth's SD uses the n divisor, and not the n-1 divisor.
Lenth.SD <- sd(Xvalues)*sqrt(length(Xvalues)-1)/sqrt(length(Xvalues))
cat("Value of SD for Lenths power analysis is ", Lenth.SD, "\n")

#--------------------------------------------------------------
# Illustration of how to wrap this in to a plyr() function to get
# the power for a range of slopes.
# This is done for both the stroup and simulation methods
# and the two power plots are overlaid on each other.

sink("monitor-example1-R-020.txt", split=TRUE)
##***part020b;
powerslope.stroup <- ddply(data.frame(Trend=seq(-.10,.10,.02)), "Trend", 
   function(x, Xvalues, Process.SD, Sampling.SD, alpha=0.05){
   # run the simulations
   myres <- slr.power.stroup(Trend=x$Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha)

   return(myres)
}, Xvalues=Xvalues, Process.SD=0, Sampling.SD=Sampling.SD, alpha=0.05)

powerslope.stroup[,c("Trend","power")]
##***part020e;
sink()

# Look at how many years are needed to detect a -5% trend/year with 80% power?
sink("monitor-example1-R-030.txt", split=TRUE)
##***part030b;
poweryears.stroup <- ddply(data.frame(Nyears=4:10), "Nyears", 
   function(x, Trend, Process.SD, Sampling.SD, alpha=0.05){
   # run the simulations
   myres <- slr.power.stroup(Trend=Trend, Xvalues=rep(0:x$Nyears,2), 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha)

   return(myres)
}, Trend=Trend, Process.SD=0, Sampling.SD=Sampling.SD, alpha=0.05)

poweryears.stroup[,c("Nyears","power")]
##***part030e;
sink()
