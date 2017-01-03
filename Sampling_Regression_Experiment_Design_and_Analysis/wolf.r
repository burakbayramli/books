# Estimating a ratio using a simple random sample.
# 2015-02-21 CJS split; plyr; as.is; ggplot

# Wildlife ecologists interested in measuring the impact of wolf
# predation on moose populations in BC obtained estimates by aerial
# counting of the population size of wolves and moose on 11
# subareas (all roughly equal size) selected as SRSWOR from a total of
# 200 subarea in the game management zone.	
# In this example, the actual ratio of wolves to moose is of interest.		
# There are 200 subareas areas in the population (this is the population size N)	

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)
library(survey)

# Read in the data
sink('wolf-R-001.txt', split=TRUE)
##***part001b;
wolf <- read.csv("wolf.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
wolf
##***part001e;
sink()

# Do a plot of moose vs wolves to check for linearity
##***part002b;
prelim <- ggplot(data=wolf, aes(x=Moose, y=Wolves))+
  ggtitle("Wolves vs Moose")+
  geom_point(size=4)
prelim
##***part002e;
ggsave(prelim, file='wolf-R-002.png', h=4, w=6, units="in", dpi=300)


#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Create the weighting variable = 1/X
sink('wolf-R-003.txt', split=TRUE)
##***part003b;
wolf$weight <- 1/wolf$Moose
wolf[1:5,]
##***part003e;
sink()


sink('wolf-R-004.txt', split=TRUE)
##***part004b;
wolf.fit <- lm(Wolves ~ 0 + Moose, data=wolf, weights=weight)
summary(wolf.fit)
##***part004e;
sink()

# Do a plot of moose vs wolves to check for linearity and add the fitted curve
##***part005b;
prelim2 <- prelim +
  geom_abline(intercept=0, slope=wolf.fit$coefficients[1])
prelim2
##***part005e;
ggsave(prelim2, file='wolf-R-005.png', h=4, w=6, units="in", dpi=300)


#************ Analysis using the survey package **********

# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.
# This is likely overkill for simple random samples and simple
# means and totals

##***part010b;
library(survey)
##***part010e;

# Add a variable for the number of subarea in the population
sink('wolf-R-011.txt', split=TRUE)
##***part011b;
wolf$TotalSubareas <- 200
wolf[1:5,]
##***part011e;
sink()

# Set up the design of the survey
sink('wolf-R-012.txt', split=TRUE)
##***part012b;
wolf.design <- svydesign(data=wolf, 
      ids=~1, # no clusters
      fpc=~TotalSubareas)  # boats in the day
print(wolf.design)
##***part012e;
sink()

# Estimate the ratio
sink('wolf-R-013.txt', split=TRUE)
##***part013b;      
est.ratio <- svyratio(numerator=~Wolves,
               denominator=~Moose, wolf.design)
est.ratio.ci <- confint(est.ratio)
est.ratio
est.ratio.ci
##***part013e;
sink()
