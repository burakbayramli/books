# Lifetime of various battery brands

# 2015-04-18 CJS remove base R; misc fix ups
# 2014-04-18 CJS ggplot, split, lsmeans update

# In Christmas 1995, we bought our son a radio-controlled 
# car. On the way to buy some batteries,
# my son asked if it made a difference which brand 
# we purchased. A learning moment presented itself!

# We purchased multiple sets of various brands 
# of batteries. We randomly tested these batteries
# over the next few weeks and recorded the time (hours) of the
# lifetime of each set.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)

# Read in the data
sink('battery-R-000.txt', split=TRUE)
##***part001b;
fun <- read.csv('battery.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
fun$brand <- factor(fun$brand)
fun
##***part001e;
sink()

##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=fun, aes(x=brand, y=lifetime))+
  ggtitle("Lifetimes of various brands of batteries")+
  xlab("Brand")+ylab("Lifetime (yr)")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='battery-R-prelim.png', h=4, w=6, units="in", dpi=300)



# Compute some summary statistics for each group using my special functions
sink('battery-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(fun, "brand", summarize,
                n.brand     = length(lifetime),
                mean.lifetime= mean(lifetime),
                sd.lifetime  = sd(lifetime))
report

report <- ddply(fun, "brand", sf.simple.summary, variable="lifetime", crd=TRUE)
report
##***part003e;
sink()


# fit the linear model and get the ANOVA table and test for effects
sink('battery-R-005.txt', split=TRUE)
##***part005b;
result <- lm(lifetime ~ brand, data=fun)
anova(result)
##***part005e;
sink()

##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='battery-R-diag.png', h=6, w=6, units="in", dpi=300)


sink('battery-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~brand, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('battery-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="brand")
plotcld <- plotcld + 
        xlab("Brand")+
        ylab("Mean lifetime (with 95% ci")+
        ggtitle("Comparison of mean lifetime with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="brand")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("Brand")+
        ylab("Mean lifetime (with 95% ci")+
        ggtitle("Comparison of mean lifetime with cld")
plotcldb
##***partcldplotse;



ggsave(plot=plotcld, file='battery-R-cldbar.png', h=4,w=6, units="in", dpi=300)
ggsave(plot=plotcldb, file='battery-R-cldline.png', h=4, w=6, units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('battery-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='battery-R-pairdiff.png', h=4, w=6, units="in", dpi=300)






