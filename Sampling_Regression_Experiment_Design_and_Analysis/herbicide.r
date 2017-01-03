# Do herbicides improve yield?
# 2015-04-17 CJS misc changes
# 2014-04-20 CJS ggplot, lsmeans,etc

# Three herbicides (and an control) were tested 
# on their effect on the yield of a crop.
# The experiment was done in four fields (fields)
# and the yield was measured at
# the end of the experiment.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusing by LaTex and usually are not coded.
#
options(useFancyQuotes=FALSE) # renders summary output corrects

# Load the necessary libraries
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)
  
#source('../../schwarz.functions.r')  # in case no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)




# Read in the data
sink('herbicide-R-000.txt', split=TRUE)
##***part000b;
growth <- read.csv('herbicide.csv', header=TRUE, as.is=TRUE)
growth
##***part000e;
sink()



sink('herbicide-R-020.txt', split=TRUE)
##***part020b;
# Declare both block (field)  and treatment (herbicide) variable  as factors
growth$herbicide  <- factor(growth$herbicide, levels=c("control","24D TCA","Dn/CR","Sesin"), order=TRUE)
growth$field      <- factor(growth$field)
str(growth)
##***part020e;
sink()


##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=growth, aes(x=herbicide, y=weight))+
  ggtitle("Weight at various herbicides")+
  xlab("Herbicide")+ylab("Weight")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='herbicide-R-prelim.png', h=4, w=6, units="in", dpi=300)



# Profile plot
##***partprofileplotb;
# Side-by-side dot and boxplots 
plotprofile <- ggplot(data=growth, 
                      aes(x=field, y=weight, group=herbicide, shape=herbicide))+
  ggtitle("weight over time")+
  xlab("Field")+ylab("weight")+
  geom_point(size=4)+
  geom_line()
plotprofile
##***partprofileplote;

ggsave(plot=plotprofile, file='herbicide-R-profile.png', h=4, w=6, units="in", dpi=300)

  

# Check for block completeness
sink('herbicide-R-checkcomplete.txt', split=TRUE)
##***partcheckcompleteb;
xtabs(~field+herbicide, data=growth)
##***partcheckcompletee;
sink()

# Compute some summary statistics for each group 
# We don't compute a se here because the design is not a CRD
sink('herbicide-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(growth, "herbicide", sf.simple.summary, variable="weight")
report
##***part003e;
sink()



# fit the linear model and get the ANOVA table and test for effects
# Note that if the design is UNbalanced (some missing data), we must fit the block term 
# first and then the treatment term -- R gives the "Type I" ss otherwise.
# You should generally put the blocking term first in the lm() models.

sink('herbicide-R-030.txt', split=TRUE)
##***part030b;
result <- lm(weight ~ field + herbicide, data=growth)
anova(result)
##***part030e;
sink()



##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='herbicide-R-diag.png', h=6, w=6, units="in", dpi=300)




sink('herbicide-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans::lsmeans(result, ~herbicide, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('herbicide-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="herbicide")
plotcld <- plotcld + 
        xlab("herbicide")+
        ylab("Mean weight (with 95% ci)")+
        ggtitle("Comparison of mean weight with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="herbicide")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("herbicide")+
        ylab("Mean weight (with 95% ci)")+
        ggtitle("Comparison of mean weight with cld")
plotcldb
##***partcldplotse;

ggsave(plot=plotcld, file='herbicide-R-cldbar.png', h=4, w=6, units="in", dpi=300)
ggsave(plot=plotcldb, file='herbicide-R-cldline.png', h=4, w=6, units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('herbicide-R-pairsreport.txt', split=TRUE)
##***partpairsb;
# Find all the pairwise differences adjusting for multipicity
result.pairs <- pairs(result.lsmo, adjust='tukey')
summary(result.pairs, infer=TRUE)
##***partpairse;
sink()


# Make a plot of the differences
##***partpairsplotb;
result.pairs.ci <- confint(result.pairs) # extract the ci values
result.pairs.ci
plotdiff <- ggplot(result.pairs.ci, aes(contrast, estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_point(size=4)+
    geom_linerange(size=1.5)+
    geom_abline(interecept=0, slope=0, linetype=2)+
    ylab("Estimated diff and 95% ci")+
    xlab("Contrast")+
    ggtitle("Estimated pairwise differences and ci")
plotdiff
##***partpairsplote;

ggsave(plot=plotdiff, file='herbicide-R-pairdiff.png', h=4, w=6, units="in", dpi=300)
