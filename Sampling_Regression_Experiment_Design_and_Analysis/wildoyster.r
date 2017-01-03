# Density of wild oysters
# 2015-02-21 CJS update as.is; split; ggplot; plyr

# A First Nation wished to develop a wild oyster fishery. 
# As first stage in the development of the fishery, 
# a survey was needed to establish
# the current stock in a number of oyster beds.


# This example looks at the estimate of oyster numbers 
# from a survey conducted in 1994.


# The survey was conducted by a line through the oyster bed --
# the total length was 105 m. Several random Transect locations were located
# along the line. At each randomly chosen Transect,
# the width of the bed was measured and about 3 random location along
# the perpendicular Transect at that point were taken. A 1 m^2 quadrat
# was applied, and the number of oysters of various sizes was counted
# in the quadrat.	

# The Transect are a cluster (randomly chosen). Within each cluster
# three points were randomly selected. This is an example of a two-stage
# design.

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)
library(survey)

# Read in the data
sink('wildoyster-R-001.txt', split=TRUE)
##***part001b;
wildoyster <- read.csv("wildoyster.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
wildoyster$BedLength <- 105
wildoyster[1:5,]
##***part001e;
sink()





#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# We need first aggregate up to the Transect level
sink('wildoyster-R-002.txt', split=TRUE)
##***part002b;
trans.wildoyster <- ddply(wildoyster, "Transect", summarize,
                  weight.mean = mean(weight),
                  weight.sd   = sd(weight),
                  weight.n    = length(weight),
                  width.mean  = mean(width),
                  BedLength.mean = mean(BedLength))
head(trans.wildoyster)
##***part002e;
sink()


# Convert the mean weight on each transect to an estimate of total weight on the transect

##***part003b;
trans.wildoyster$weight.total <- trans.wildoyster$weight.mean * 
          trans.wildoyster$width.mean
##***part003e;

sink('wildoyster-R-004.txt', split=TRUE)
##***part004b;
trans.wildoyster$weight.var.2nd <-
      trans.wildoyster$weight.sd**2/trans.wildoyster$weight.n * 
      (1- trans.wildoyster$weight.n/trans.wildoyster$width.mean) *
      trans.wildoyster$width.mean**2 *
      trans.wildoyster$BedLength.mean / nrow(trans.wildoyster)
trans.wildoyster
##***part004e;
sink()

# Now to estimate the mean weight at the PSU level and expand to the population
##***part005b;
mean.trans.weight.total    <- mean(trans.wildoyster$weight.total)
est.weight.total    <- mean.trans.weight.total    * 105
##***part005e;

# Find the variance from the first stage of sampling
##***part006b;
var.est.weight.total.1st <- sd(trans.wildoyster$weight.total)**2/nrow(trans.wildoyster) *
      (1-nrow(trans.wildoyster)/105) * 105**2
##***part006e;

# Find the total se from both stage
sink('wildoyster-R-007.txt', split=TRUE)
##***part007b;
se.est.weight.total <- sqrt( sum(trans.wildoyster$weight.var.2nd) + var.est.weight.total.1st)
 
cat('Estimated total weight is ',est.weight.total,
    ' with an approximate se of ', se.est.weight.total, "\n")
##***part007e;
sink() 
 
    
    

#************ Analysis using the survey package **********

# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.

sink('wildoyster-R-020.txt', split=TRUE)
##***part020b;
library(survey)
wildoyster.design <- svydesign(data=wildoyster, 
      ids=~Transect+quadrat, # Transects are clusters
      fpc=~BedLength+width)  # no fpc
print(wildoyster.design)
est.total <- svytotal(~weight, design=wildoyster.design)
est.total
##***part020e;
sink()
