# Example of power analysis for to detect a change in slope between and after a KNOWN breakpoint
# 2014-11-21 CJS Removed reference to Ivalue as not needed
# 2014-11-02 CJS First edition

# A large industrial firm plans to implement a water treatment plan in after 2014 but before 2015
# to try and stop the accumulation of a chemical in the watershed that is release as a consequnce of
# operations.
#
# They have readings of the chemical in the tissue of inidivual fish taken in several years prior to 
# the implementation of the plan.


library(ggplot2)
library(lmerTest)
library(plyr)
source("reg.power.ChangeSlopeAfterBreakpoint.stroup.r")

options(width=300)
# Read in the individual fish data from the site
fish.raw.csv <- textConnection(
"Year,  Chem 
  2001 , 9.4 
  2001 , 7.9 
  2001 , 8.1 
  2001 , 8.1 
  2001 , 10.21 
  2001 , 8.2 
  2002 , 8.5 
  2002 , 7.7 
  2002 , 9.2 
  2002 , 6.5 
  2002 , 7.9 
  2002 , 6.5 
  2002 , 7.2 
  2002 , 8.5 
  2003 , 6.5 
  2003 , 7.2 
  2003 , 7.0 
  2003 , 6.9 
  2003 , 6.9
  2006 , 10.2 
  2006 , 11.3 
  2006 , 10.1 
  2006 , 12.2 
  2006 , 12.1 
  2009 , 13.2 
  2009 , 12.7
  2009 , 12.4
  2009 , 10.7
  2009 , 12.5 
  2012 , 9.3
  2012 , 8.2
  2012 , 8.6
  2012 , 7.5
  2012 , 8.5"  )

fish.raw <- read.csv(fish.raw.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
fish.raw

fish.raw$logChem <- log(fish.raw$Chem)
fish.raw$YearF   <- factor(fish.raw$Year)

# Make a plot.
ggplot(data=fish.raw, aes(x=Year, y=logChem))+
  ggtitle("Concentration  over time")+
  scale_x_continuous(breaks=min(fish.raw$Year):max(fish.raw$Year))+
  geom_point(position=position_jitter(w=0.1))+
  stat_smooth(method="lm", se=FALSE)

# Extract the current trend and 
# Extract out the process and sampling error after adjusting for a linear trend

fish.fit <- lmerTest:: lmer( logChem ~ Year + (1|YearF), data=fish.raw)
summary(fish.fit)
fish.vc <- as.data.frame(VarCorr(fish.fit))
fish.vc

# What is the average number of fish sampled per year?
afishpyear <- nrow(fish.raw)/length(unique(fish.raw$Year))
cat("average number fish sampled per year is ", afishpyear, "\n")

Trend.before.breakpoint <- fixef(fish.fit)[2]
names(Trend.before.breakpoint) <- NULL
cat("Trend  (on log-scale) prior to breakpoint in 2014.5 is ", Trend.before.breakpoint, "\n")



# Now for some power scenarios.
# We wish to see how many years are needed to detect that trend has gone to 0?

Trend.after.breakpoint  <- 0  # assume that accumulation stops
BreakPoint              <- 2014.5
Process.SD  <- fish.vc[1,"sdcor"]
Sampling.SD <- fish.vc[2,"sdcor"]
cat('Estimated process and  sampling errors of raw data on log-scale is ', Process.SD, Sampling.SD, "\n")
# Given the rather large process error, it be VERY hard to detect any changes in the slope.


results <- ddply(data.frame(FinalYear=2015:2030), "FinalYear", function(x){  # loop over the years post implementation
   Xvalues <- c( fish.raw$Year, rep(2015:x$FinalYear, each=ceiling(afishpyear)))
   # Find the power for a halt in the trend, assuming the approximate same number of fish sampled
   # per year in the area in post breapoint years as before the breakpoint.
   reg.power.ChangeSlopeAfterBreakpoint.stroup(
     Trend.before.breakpoint, BreakPoint, Trend.after.breakpoint, 
     Xvalues, Process.SD, Sampling.SD, alpha=0.05)
})

# THe value of beta1, beta2, and beta3 are the intercept, trend before breakpoint, and change in trend
# after the break point respectively. So if the trend is to be halted, the change must be
# the same as original trend, but in the negative direction to give a net (new) trend of zero.
results

# rather depressing but not unexpected given the large-process error in the data
ggplot(data=results, aes(x=FinalYear, y=os.power1))+
  ggtitle("Power to detect change in slope to 0 slope after breakpoint in 2014.5")+
  ylab("Power to detect CHANGE in trend to 0 slope after breakpoint in 2014.5")+
  xlab("Year post implementation in 2014.5")+
  ylim(0,1)+
  geom_hline(yintercept=.80, linetype=2)+
  geom_line()

