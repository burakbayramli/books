# Estimating the catch of sockeye in a stratified design.
# 2015-02-21 CJS ##--- to ##***, split=TRUE; plyr package

# On each of two days, a sample of vessels were assigned observers
# who counted the number of sockeye salmon caught in that day. 
# On the second day, a new set of vessels was observed.
# On each day, there were 250 vessels that participated in the survey.
 
# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.


library(plyr)
library(survey)

# Read in the data
sink('sockeye-R-001.txt', split=TRUE)
##***part001b;
sockeye <- read.csv("sockeye.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
sockeye[1:12,]
##***part001e;
sink()
str(sockeye)

#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Find the mean, sd, and se of the catch/boat for each stratum

sink('sockeye-R-002.txt', split=TRUE)
##***part002b;
catch.stat    <- ddply(sockeye, "date", summarize, 
                        n   =length(sockeye),
                        mean=mean  (sockeye),
                        sd  =sd    (sockeye))

# Compute the se for each stratum, ignoring the fpc.
# Note that we assume each stratum is sampled using an SRS here
catch.stat$se.mean <- catch.stat$sd / sqrt(catch.stat$n)
catch.stat
##***part002e;
sink()



# Find the total in each stratum and its se by expansion.
sink('sockeye-R-004.txt', split=TRUE)
##***part004b;
# Create a data frame for the expansion factors for each stratum
ExpFactor <- data.frame(date=c('29-Jul','30-Jul'),TotalBoats=c(250,250))
catch.stat <- merge(catch.stat, ExpFactor)
catch.stat$TotalCatch    <- catch.stat$mean    * catch.stat$TotalBoats
catch.stat$se.TotalCatch <- catch.stat$se.mean * catch.stat$TotalBoats
catch.stat
##***part004e;
sink()
 

# Now for the grand total
sink('sockeye-R-005.txt', split=TRUE)
##***part005b;

Overall.Catch    <- sum(catch.stat$TotalCatch)
se.Overall.Catch <- sqrt(sum(catch.stat$se.TotalCatch**2))
cat("Estimated grand total is ",Overall.Catch,
    ';\n with an estimated se of ',se.Overall.Catch,"\n") 
##***part005e;
sink()






# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.
# This is likely overkill for simple random samples and simple
# means and totals

##***part010b;
library(survey)
##***part010e;

# We add the population size (number of boats to each stratum)
# The ExpFactor was defined for each stratum 
sink('sockeye-R-011.txt', split=TRUE)
##***part011b;
ExpFactor <- data.frame(date=c('29-Jul','30-Jul'),TotalBoats=c(250,250))
sockeye <- merge(sockeye, ExpFactor) 
sockeye[1:5,]
##***part011e;
sink()

# We define the survey design
sink('sockeye-R-012.txt', split=TRUE)
##***part012b;
sockeye.design <- svydesign(data=sockeye, 
      ids=~1, # no clusters
      strata=~date,
      fpc=~TotalBoats)  # boats in the day
print(sockeye.design)
##***part012e;
sink()


sink('sockeye-R-013.txt', split=TRUE)
##***part013b;      
# Estimate the mean and total catch/boat for each strata
str.means <- svyby(~sockeye, by=~date, design=sockeye.design, svymean)
str.means
str.totals<- svyby(~sockeye, by=~date, design=sockeye.design, svytotal)
str.totals
##***part013e;
sink()





sink('sockeye-R-014.txt', split=TRUE)
##***part014b;
# Estimate the grand totals
est.totals    <- svytotal(~sockeye, sockeye.design)
est.totals.ci <- confint(est.totals)
est.totals
est.totals.ci
report.totals <- cbind( data.frame(mean=est.totals),data.frame(est.totals.ci))
report.totals
##***part014e;
sink()
