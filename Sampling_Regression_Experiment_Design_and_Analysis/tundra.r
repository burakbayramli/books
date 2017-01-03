# Estimating the number of tundra swans
# 2015-02-21 CJS Change all ##--- to ##***
# 2014-01-30 CJS Updated split=TRUE
#                Used plyr() package; 

# The Tundra Swan {Cygnus columbianus}, formerly known as the
# Whistling Swan, is a large bird with white plumage and black legs, 
# feet, and beak. Additional information about the tundra  
# swan is available at http://www.hww.ca/hww2.asp?id=78&cid=7. 
# The USFWS is responsible for conserving and protecting tundra swans 
# as a migratory bird under the Migratory Bird Treaty Act and the Fish 
# and Wildlife Conservation Act of 1980. As part of these 
# responsibilities, it conducts regular aerial surveys at one of their 
# prime breeding areas in Bristol Bay, Alaska. And, the Bristol Bay 
# population of tundra swans is of particular interest because suitable 
# habitat for nesting is available earlier than most other nesting 
# areas. This example is based on one such survey conducted by Doster, J. 
# (2002, Tundra Swan Population Survey in Bristol Bay, Northern Alaska 
# Peninsula, June 2002.)

# Tundra swans are highly visible on their nesting grounds making them 
# easy to monitor during aerial surveys.

# The Bristol Bay refuge has been divided into 186 survey units, each 
# being a quarter section. These survey units have been divided into 
# three strata based on density, and previous years' data provide the 
# following information about the strata:

# Density     Total             
# Stratum     Survey    Past         Past
#             Units     Density     Std Dev 
# ------------------------------------------------
#   High        60       20           10
#   Medium      68       10            6
#   Low         58        2            3
# ------------------------------------------------
# Total        186      
 

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(plyr)
library(survey)

# Read in the data
sink('tundra-R-001.txt', split=TRUE)
##***part001b;
tundra <- read.csv("tundra.csv", header=TRUE)
tundra[1:12,]
##***part001e;
sink()


#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Find the mean, sd, and se of the average swans/grid square for each stratum

sink('tundra-R-002.txt', split=TRUE)
##***part002b;
library(plyr)
summary.Swans <- ddply(tundra, "stratum", function(x){
  # Compute statistics for each stratum
  # We assume that each stratum has an SRS conducted
  # The fpc is ignored.
      n    <- length(x$all.swans)
      mean <- mean  (x$all.swans)
      sd   <- sd    (x$all.swans)
      se.mean <- sd/sqrt(n)
      res  <- c(n,mean,sd,se.mean) # results
      names(res)<- c("n","mean","sd","se.mean")
      return(res)      
})
summary.Swans
##***part002e;
sink()



# Find the total in each stratum and its se by expansion.
sink('tundra-R-004.txt', split=TRUE)
##***part004b;
# Create a data frame for the expansion factors for each stratum
ExpFactor <- data.frame(stratum=c('h','l','m'),TotalGrids=c(60,58,68))
summary.Swans <- merge(summary.Swans, ExpFactor)
summary.Swans$TotalSwans <- summary.Swans$mean * summary.Swans$TotalGrids
summary.Swans$se.TotalSwans <- summary.Swans$se.mean * summary.Swans$TotalGrids
summary.Swans
##***part004e;
sink()
 

# Now for the grand total
sink('tundra-R-005.txt', split=TRUE)
##***part005b;
Overall.Swans    <- sum(summary.Swans$TotalSwans)
se.Overall.Swans <- sqrt(sum(summary.Swans$se.TotalSwans**2))
cat("Estimated grand total is ",Overall.Swans,
    ';\n with an estimated se of ',se.Overall.Swans,"\n") 
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

# We add the population size (number of grid squares to each stratum)
# The ExpFactor was defined for each stratum 
sink('tundra-R-011.txt', split=TRUE)
##***part011b;
ExpFactor <- data.frame(stratum=c('h','l','m'),TotalGrids=c(60,58,68))
tundra <- merge(tundra, ExpFactor) 
tundra[1:5,]
##***part011e;
sink()

# We define the survey design
sink('tundra-R-012.txt', split=TRUE)
##***part012b;
tundra.design <- svydesign(data=tundra, 
      ids=~1, # no clusters
      strata=~stratum,
      fpc=~TotalGrids)  # grid squares in stratum
print(tundra.design)
##***part012e;
sink()


sink('tundra-R-013.txt', split=TRUE)
##***part013b;      
# Estimate the mean and total Swans/grid square for each strata
str.means <- svyby(~all.swans, by=~stratum, design=tundra.design, svymean)
str.means
str.totals<- svyby(~all.swans, by=~stratum, design=tundra.design, svytotal)
str.totals
##***part013e;
sink()





sink('tundra-R-014.txt', split=TRUE)
##***part014b;
# Estimate the grand totals
est.totals    <- svytotal(~all.swans, tundra.design)
est.totals.ci <- confint(est.totals)
est.totals
est.totals.ci
report.totals <- cbind( data.frame(mean=est.totals),data.frame(est.totals.ci))
report.totals
##***part014e;
sink()
 

