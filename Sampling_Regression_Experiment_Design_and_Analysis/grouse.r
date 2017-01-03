# Estimating numbers of grouse using a survey	
# 2015-02-21 CJS update split; ##***; ggplot2


# A wildlife biologist has estimated the grouse population
# in a region containing isolated areas (called pockets) of
# bush as follows: She selected 12 pockets of bush at random, and
# attempted to count the numbers of grouse in each of of these.
# (One can assume that the grouse are almost all found in the bush, and for the
# purpose of this question, that the counts were perfectly accurate.)	
# The total number of pockets of bush in the region is 248, 
# comprising a total area of 3015 hectares.	

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)
library(survey)

# Read in the data
sink('grouse-R-001.txt', split=TRUE)
##***part001b;
grouse <- read.csv("grouse.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
Npockets <- 248
TotalArea <- 3015
grouse
##***part001e;
sink()



# Analysis as a simple random sample, ignoring the area
sink('grouse-R-002.txt', split=TRUE)
##***part002b;
n.pockets        <- length(grouse$grouse)
mean.grouse      <- mean(grouse$grouse)
sd.grouse        <- sd(grouse$grouse)
se.mean.grouse   <- sd.grouse / sqrt(n.pockets)
cat(' Est Mean grouse/pocket is ',mean.grouse,' with a se of',
      se.mean.grouse,"\n")
      
total.grouse    <- mean.grouse    * Npockets
se.total.grouse <- se.mean.grouse * Npockets
cat(' Est Total grouse is ',total.grouse,' with a se of',
      se.total.grouse,"\n")
##***part002e;
sink()


# Analysis as a simple random sample, using the survey package
sink('grouse-R-003.txt', split=TRUE)
##***part003b;
library(survey)
# Add a variable for the number of subarea in the population
grouse$Npockets <- Npockets
grouse[1:5,]
grouse.design <- svydesign(data=grouse, 
      ids=~1, # no clusters
      fpc=~Npockets)  # boats in the day
print(grouse.design)
mean.grouse2  <- svymean(  ~grouse, design=grouse.design)
mean.grouse2
total.grouse2 <- svytotal( ~grouse, design=grouse.design)
total.grouse2
##***part003e;
sink()










#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Create the weighting variable = 1/X
sink('grouse-R-010.txt', split=TRUE)
##***part010b;
grouse$weight <- 1/grouse$area
grouse[1:5,]
##***part010e;
sink()


# Do a plot of grouse vs area to check for linearity
png('grouse-R-011.png')
##***part011b;
plot(grouse$area, grouse$grouse,
   main='grouse vs area')
##***part011e;
dev.off()

##***part011b;
prelim <- ggplot(data=grouse, aes(x=area, y=grouse))+
  ggtitle("grouse vs. area")+
  geom_point(size=4)
prelim
##***part011e;
ggsave(prelim, file='grouse-R-011.png', h=4, w=6, units="in", dpi=300)


sink('grouse-R-012.txt', split=TRUE)
##***part012b;
grouse.fit <- lm(grouse ~ 0 + area, data=grouse, weights=weight)
summary(grouse.fit)
##***part012e;
sink()

# Do a plot of area vs grouse to check for linearity and add the fitted curve
##***part013b;
prelim2 <- prelim +
  geom_abline(intercept=0, slope=grouse.fit$coefficients[1])
prelim2
##***part013e;
ggsave(prelim2, file='grouse-R-013.png', h=4, w=6, units="in", dpi=300)

# Ge the estimated total
sink('grouse-R-014.txt', split=TRUE)
##***part014b;
total.grouse.ratio    <- grouse.fit$coefficient * TotalArea
se.total.grouse.ratio <- coef(summary(grouse.fit))[,"Std. Error"] * TotalArea 
cat(' Est Total grouse using ratio estimator is ',total.grouse.ratio,' with a se of',
      se.total.grouse.ratio,"\n")

##***part014e;
sink()


#************ Analysis using the survey package **********

# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.
# This is likely overkill for simple random samples and simple
# means and totals

sink('grouse-R-020.txt', split=TRUE)
##***part020b;
library(survey)
grouse$Npockets <- Npockets
grouse[1:5,]
grouse.design <- svydesign(data=grouse, 
      ids=~1, # no clusters
      fpc=~Npockets)  # boats in the day
print(grouse.design)
grouse.ratio <- svyratio(numerator=~grouse,
               denominator=~area, grouse.design)
grouse.ratio
grouse.total <- predict(grouse.ratio, total=TotalArea)
grouse.total
##***part020e;
sink()
