# Estimating density of urchins using a transect survey.
# 2015-02-21 CJS update ggplot; plyr; split; as.is




# This dataset consists of the results from a series
# of transects conducted perpendicular to the shore.
# As divers swam along the transect, they counted
# the number of red sea urchins in 1 m**2 quadrats.

# The variables are (from left to right):
#    transect, quadrat, legal size, sublegal sized.


# If a quadrat is not present in this listing, then the count was 0
# for both variables. It does NOT indicate that the quadrat
# was not measured - rather that no urchins were found.


# There was no transect numbered 5, 12, 17, or 19.	

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)
library(survey)


# Read in the data
sink('urchin-R-001.txt', split=TRUE)
##***part001b;
urchin <- read.csv("urchin.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
urchin$total <- urchin$legal + urchin$sublegal
urchin[1:5,]
##***part001e;
sink()



# We need first aggregate up to the transect level
sink('urchin-R-002.txt', split=TRUE)
##***part002b;
trans.urchin <- ddply(urchin, "transect", summarize,
            n.quad  = length(legal),
            legal.sum = sum(legal),
            quad.min  = min(quad),
            quad.max  = max(quad))
trans.urchin
# Check to see that length = max - min (see if anything missing)
cat('Check to see if any missing quadrat information\n')
trans.urchin$n.quad - (trans.urchin$quad.max-trans.urchin$quad.min+1)
##***part002e;
sink()


#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Do a plot of urchin vs area to check for linearity
##***part011b;
prelim <- ggplot(data=trans.urchin, aes(x=n.quad, y=legal.sum))+
  ggtitle('# Urchin vs length of transect')+
  geom_point(size=4)
prelim
##***part011e;
ggsave(prelim, file='urchin-R-011.png', h=4, w=6, units="in", dpi=300)



sink('urchin-R-012.txt', split=TRUE)
##***part012b;
urchin.fit <- lm(legal.sum ~ 0 + n.quad, data=trans.urchin, 
              weights=1/n.quad)
summary(urchin.fit)
##***part012e;
sink()

# Do a plot of area vs urchin to check for linearity and add the fitted curve
##***part013b;
prelim2 <- prelim +
  geom_abline(intercept=0, slope=urchin.fit$coefficients)
prelim2
##***part013e;
ggsave(prelim2, file='urchin-R-013.png',  h=4, w=6, units="in", dpi=300)


#************ Analysis using the survey package **********

# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.

sink('urchin-R-020.txt', split=TRUE)
##***part020b;
library(survey)
urchin.design <- svydesign(data=trans.urchin, 
      ids=~transect, # transects are clusters
      fpc=NULL)  # no fpc
print(urchin.design)
urchin.ratio <- svyratio(numerator=~legal.sum,
               denominator=~n.quad, urchin.design)
urchin.ratio
##***part020e;
sink()
