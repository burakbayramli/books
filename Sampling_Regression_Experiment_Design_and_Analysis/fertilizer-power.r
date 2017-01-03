
# Example of power analysis for simple linear regression 
# 2014-11-21 CJS Removed the Ivalue as not needed
# 2014-07-18 CJS First edition

# Let us return to the example of the yield of tomatoes vs. the amount of fertilizer. 
# We wish to design an experiment to detect a slope of 1 (the effect size). 
# From past data, the standard deviation of
# values about the regression line is about 2 units (the standard deviation of the residuals). 

# We have enough money to plant 12 plots with levels of fertilizer ranging from 10 to 20. 
# How does the power compare under different configuration
# of choices of fertilizer levels. More specifically, how does the power compare between using
# fertilizer levels 
#     (10, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 20), i.e. an even distribution of
#        levels of fertilizer, and 
#     (10, 10, 12, 12, 14, 14, 16, 16, 18, 18, 20, 20), i.e. doing two replicates
#        at each level of fertilizer but doing fewer distinct levels?

# There are two methods for determining power - based on methods by Stroup (1999) or
# via simulation. Both are illustrated here.

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(width=200)            # wide output lines

library(ggplot2)


set.seed(12343124) # only needed if the simulaton method is used.

source("../SLR-power/slr-power-stroup.r") # load the power function
source("../SLR-power/slr-power-sim.r")     # load the power function that does simulation


#----------------------------------------------------------------
# The power functions allow you specify both process and sampling error.
# For the fertilizer experiment, process error is assumed to be zero because
# we assume that each plot's growth is independent of any other plot and
# that all the plots are grown in the same growing season.

# We will do it using both the Stroup and Simulation method.

# Case 1
options(width=60)
sink("fertilizer-power-R-010.txt", split=TRUE)
##***part010b;

Xvalues <- c(10, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 20)

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=0, Sampling.SD=4, alpha=0.05)
slr.power.sim   (Trend=1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=0, Sampling.SD=4, alpha=0.05, nsim=1000)
##***part010e;
sink()

# To use the Lenth power analysis page for regression the following settings are used
#    1 predictor
#    SD of xvalues for Lenth's program requires the SD of the X values to be computed using 
#       the n divisor rather than the n-1 divisior
cat("SD for Lenth power analysis is : ",sd(Xvalues)*sqrt((length(Xvalues)-1))/sqrt(length(Xvalues)),"\n") # 
# Error SD is 4
# Number of data points is 
cat("Number of data points is ", length(Xvalues), "\n")

#-------------------
# Case 2
sink("fertilizer-power-R-020.txt", split=TRUE)
##***part020b;

Xvalues <- c(10, 10, 12, 12, 14, 14, 16, 16, 18, 18, 20, 20)

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=0, Sampling.SD=4, alpha=0.05)
slr.power.sim   (Trend=1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=0, Sampling.SD=4, alpha=0.05, nsim=1000)
##***part020e;
sink()

# To use the Lenth power analysis page for regression the following settings are used
#    1 predictor
#    SD of xvalues for Lenth's program requires the SD of the X values to be computed using 
#       the n divisor rather than the n-1 divisior
cat("SD for Lenth power analysis is : ",sd(Xvalues)*sqrt((length(Xvalues)-1))/sqrt(length(Xvalues)),"\n") # 
# Error SD is 4
# Number of data points is 
cat("Number of data points is ", length(Xvalues), "\n")



#--------------------------------------------------------------
# Illustration of how to wrap this in to a plyr() function to get
# the power for a range of slopes.
# This is done for both the stroup and simulation methods
# and the two power plots are overlaid on each other.


powerslope.stroup <- ddply(data.frame(Trend=seq(0,2,.25)), "Trend", 
   function(x, Xvalues, Process.SD, Sampling.SD, alpha=0.05){
   # run the simulations
   myres <- slr.power.stroup(Trend=x$Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha)

   return(myres)
}, Xvalues=Xvalues, Process.SD=0, Sampling.SD=4, alpha=0.05)

powerslope.stroup


powerslope.sim <- ddply(data.frame(Trend=seq(0,2,.25)), "Trend", 
   function(x, Xvalues, Process.SD, Sampling.SD, alpha=.05, nsim=1000){
   # run the simulations
   myres <- slr.power.sim(Trend=x$Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha, nsim=nsim)

   return(myres)
}, Xvalues=Xvalues, Process.SD=0, Sampling.SD=4, alpha=0.05, nsim=1000)

powerslope.sim

powerplot <- ggplot(data=powerslope.stroup, aes(x=Trend, y=power))+
  ggtitle(paste("Estimated power curve for the slope\nAnalytical and Simulated power values\nX values",
                paste(Xvalues,collapse=" ")))+
  xlab("Trend")+ylab("Power")+ylim(c(0,1))+
  geom_point()+
  geom_line(group=1)+
  geom_line(data=powerslope.sim, aes(x=Trend, y=Power.slope), group=1, color="blue")+
  geom_hline(yintercept=0.80, color="red", linetype=2)
powerplot

ggsave(plot=powerplot, file="fertilizer-power-R-plot.png",
       width=6, height=4, units="in")
