# Phosphor levels in varieties of fruit trees.
# 2015-04-18 CJS remove doby; misc changes
# 2014-04-19 CJS ggplot, lsmeans, etc

# Random samples of trees from three different 
# varieties were selected within an orchard.
# From each tree, leaves were selected, composited 
# and a reading of the phosphorus level for each 
# tree was found.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case internet not available
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


sink('phosphor-R-000.txt', split=TRUE)
# Read in the data
##***part001b;
trees <- read.csv('phosphor.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
trees$variety <- as.factor(trees$variety)
trees
##***part001e;
sink()

##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=trees, aes(x=variety, y=phosphor))+
  ggtitle("Phosphor levels in various varieties")+
  xlab("Variety")+ylab("Phosphor level")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2,outlier.shape=NA)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='phosphor-R-prelim.png', h=4, w=6, units="in",dpi=300)



# Compute some summary statistics for each group 
sink('phosphor-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(trees, "variety", summarize,
                n.treess = length(phosphor),
                mean.phos= mean(phosphor),
                sd.phos  = sd(phosphor))
report

report <- ddply(trees, "variety", sf.simple.summary, variable="phosphor", crd=TRUE)
report
##***part003e;
sink()



# fit the linear model and get the ANOVA table and test for effects
sink('phosphor-R-005.txt', split=TRUE)
##***part005b;
result <- lm(phosphor ~ variety, data=trees)
anova(result)
##***part005e;
sink()


##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='phosphor-R-diag.png',h=6, w=6, units="in", dpi=300)



sink('phosphor-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~variety, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()

# You can also get the estimates of the marginal means adjusted for simultaneous coverage
cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")

sink('phosphor-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="variety")
plotcld <- plotcld + 
        xlab("variety")+
        ylab("Mean phosphor (with 95% ci)")+
        ggtitle("Comparison of mean phosphor with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="variety")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("variety")+
        ylab("Mean phosphor (with 95% ci)")+
        ggtitle("Comparison of mean phosphor with cld")
plotcldb
##***partcldplotse;

ggsave(plot=plotcld,  file='phosphor-R-cldbar.png', h=4, w=6, units="in", dpi=300)
ggsave(plot=plotcldb, file='phosphor-R-cldline.png', h=4, w=6, units="in", dpi=300)

sink('phosphor-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='phosphor-R-pairdiff.png', h=4,w=6, units="in", dpi=300)
