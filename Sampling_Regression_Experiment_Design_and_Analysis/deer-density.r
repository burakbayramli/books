#  Separating process and sampling error
#  2014-11-11 CJS Removed Ivalue from call lists
#  2014-07-29 CJS First Edition
options(useFancyQuotes=FALSE) # renders summary output corrects
options(width=200)            # wide output lines

library(ggplot2)
library(plyr)


# For example, consider a study to monitor the density of white-tailed deer obtained by distance sampling on
# Fire Island National Seabird (Underwood et al, 1998), presented as the example on the spreadsheet to separate
# process and sampling variation.
# Taken from the Program Monitor website

density.csv <- textConnection("
Year , Density , SE 
1995 , 79.6 ,  23.47 
1996 , 90.1 , 11.67 
1997 , 107.1 , 12.09 
1998 , 74.1 , 10.45 
1999 , 64.2 , 13.90 
2000 , 40.8 , 12.38 
2001 , 41.2 ,   7.40 ")

density <- read.csv(density.csv, as.is=TRUE, header=TRUE, strip.white=TRUE)
density


denplot <- ggplot(data=density, aes(x=Year, y=Density))+
  ggtitle("Deer density over time")+
  ylab("Density and 95% ci")+
  geom_point()+
  geom_errorbar(aes(ymin=Density-2*SE, ymax=Density+2*SE), width=0.1)
denplot

# Separate out process and sampling SD
total.var <- var(density$Density)
cat("Total variance is ", total.var, "\n")

avg.sqSE <- mean(density$SE^2) # Note we average the square of SE
avg.SE   <- sqrt(avg.sqSE)
cat("Average SE^2", avg.sqSE, "and the sampling.SD is ", avg.SE,"\n")

process.SD <- sqrt(max(0, total.var - avg.sqSE))
cat("Process.SD is ", process.SD, "\n")


#--------------------------------------------------------------------------------
# Do a power analysis 
# -----------------------------------------------------------------------------

# Get the power analysis modules
source("../../Power/RegPower/SLR-power/slr-power-stroup.r") # load the power function

# Case 1
options(width=60)
sink("deer-density-R-010.txt", split=TRUE)
##***part010b;
Xvalues <- 0:9  # years in the study
Sampling.SD <- .20 # 20% cv at each year
Process.SD  <- .29
Trend       <- -.05 # 5% decline/year

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)
##***part010e;
sink()


#--------------------------------------------------------------
# Illustration of how to wrap this in to a plyr() function to get
# the power for a range of slopes.
# This is done for both the stroup and simulation methods
# and the two power plots are overlaid on each other.

sink("deer-density-R-020.txt", split=TRUE)
##***part020b;
powerslope.stroup <- ddply(data.frame(Trend=seq(-.10,.10,.02)), "Trend", 
   function(x, Xvalues, Process.SD, Sampling.SD, alpha=0.05){
   # run the simulations
   myres <- slr.power.stroup(Trend=x$Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha)

   return(myres)
}, Xvalues=Xvalues, Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)

powerslope.stroup[,c("Trend","power")]
##***part020e;
sink()

# Look at how many years are needed to detect a -5% trend/year with 80% power?
sink("deer-density-R-030.txt", split=TRUE)
##***part030b;
poweryears.stroup <- ddply(data.frame(Nyears=10:20), "Nyears", 
   function(x, Trend, Process.SD, Sampling.SD, alpha=0.05){
   # run the simulations
   myres <- slr.power.stroup(Trend=Trend, Xvalues=rep(0:x$Nyears,2), 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=alpha)

   return(myres)
}, Trend=Trend, Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)

poweryears.stroup[,c("Nyears","power")]
##***part030e;
sink()



#-------------------------------------------------------------------------------
# Changing effort in each year
# We want to take 4 measurements each year of year density (or equivalently, increase the sampling
# effort within a year by a factor of 4.) This will have the sampling.SD, but won't change the process.SD.

options(width=60)
sink("deer-density-R-310.txt", split=TRUE)
##***part310b;
Xvalues <- 0:9  # years in the study
Sampling.SD <- .20
Process.SD  <- .29
Trend       <- -.05 # 5% decline/year

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD/2, alpha=0.05)
##***part310e;
sink()

# Alternatively, we specifyt the X values repeated 4 imes.
options(width=60)
sink("deer-density-R-320.txt", split=TRUE)
##***part320b;
Xvalues <- rep(0:9,4)  # years in the study
Sampling.SD <- .20
Process.SD  <- .29
Trend       <- -.05 # 5% decline/year

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)
##***part320e;
sink()


# --------- Reduce the sampling SD to 0 
options(width=60)
sink("deer-density-R-120.txt", split=TRUE)
##***part120b;
Xvalues <- 0:9  # years in the study
Sampling.SD <- .20 
Process.SD  <- .29
Trend       <- -.05 # 5% decline/year

cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=Trend, Xvalues=Xvalues, 
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD/10000000, alpha=0.05)
##***part120e;
sink()

