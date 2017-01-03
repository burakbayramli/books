# Effect of salt on biomass
# 2015-04-18 CJS misc changes
# 2014-04-20 CJS ggplot, lsmeans, etc.

# An experiment was conducted where different amounts 
# of salt (ppm) were added to plots and the resulting 
# biomass of grass was measured. The experiment was 
# replicated in four blocks.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

# Load the necessary libraries
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)

#source('../../schwarz.functions.r') # in case internet not availabe
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)

cat("Effect of salt on biomass", date(),"\n")

sink('salt-R-000.txt', split=TRUE)
# Read in the data
##***part000b;
marsh <- read.csv('salt.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
print(marsh[1:12,])
##***part000e;
sink()

# Declare both block and trt variable  as factors
sink('salt-R-020.txt', split=TRUE)
##***part020b;
marsh$salt   <- factor(marsh$salt)
marsh$block  <- factor(marsh$block)
str(marsh)
##***part020e;
sink()


##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=marsh, aes(x=salt, y=biomass))+
  ggtitle("biomass at various salts")+
  xlab("salt")+ylab("biomass")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='salt-R-prelim.png', h=4, w=6, units="in",dpi=300)


##***partprofileplotb;
# Profile plot
plotprofile <- ggplot(data=marsh, 
                      aes(x=salt, y=biomass, group=block, shape=block))+
  ggtitle("biomass over salt concentrations")+
  xlab("salt concentrations")+ylab("biomass")+
  geom_point(size=4)+
  geom_line()
plotprofile
##***partprofileplote;

ggsave(plot=plotprofile, file='salt-R-profile.png', h=4, w=6, units="in",dpi=300)



# Check for block completeness
sink('salt-R-checkcomplete.txt', split=TRUE)
##***partcheckcompleteb;
xtabs(~block+salt, data=marsh)
##***partcheckcompletee;
sink()

# Compute some summary statistics for each group 
# We don't compute a se here because the design is not a CRD
sink('salt-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(marsh, "salt", sf.simple.summary, variable="biomass")
report
##***part003e;
sink()




# fit the linear model and get the ANOVA table and test for effects
# Note that if the design is UNbalanced (some missing data), we must fit the block term 
# first and then the treatment term -- R gives the "Type I" (incremental tests) ss otherwise
# and the test for a treatment effect will not be adjusted for blocks. Contact me for more
# details.

sink('salt-R-030.txt', split=TRUE)
##***part030b;
result <- lm(biomass ~ block + salt, data=marsh)
anova(result)
##***part030e;
sink()



##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='salt-R-diag.png', h=6, w=6, units="in",dpi=300)



sink('salt-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans::lsmeans(result, ~salt, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('salt-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="salt")
plotcld <- plotcld + 
        xlab("salt")+
        ylab("Mean biomass (with 95% ci)")+
        ggtitle("Comparison of mean biomass with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="salt")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("salt")+
        ylab("Mean biomass (with 95% ci)")+
        ggtitle("Comparison of mean biomass with cld")
plotcldb
##***partcldplotse;

ggsave(plot=plotcld, file='salt-R-cldbar.png', h=4, w=6, units="in",dpi=300)
ggsave(plot=plotcldb, file='salt-R-cldline.png', h=4, w=6, units="in",dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('salt-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='salt-R-pairdiff.png', h=4, w=6, units="in",dpi=300)







#------------------------------------ Blocks as a random effect -------------------------------
#
# I usually like to model blocks as random effects because if you treat them as fixed effects
# you are assuming that you would reuse the same blocks if you repeated the experiment.
#
# Use the lmer() function to fit this model and do the fun stuff.
# Use the lmerTest package to get the p-values from lmer() using the KR approximation etc

library(lme4)
library(lmerTest)  # you must load in the proper order (R is free, but not cheap)
result.lmerTest <- lmerTest::lmer(biomass ~ salt + (1 | block), data=marsh)

# Notice that the p-value for the salt effect is the same regardless if blocks
# are fixed or random
lmerTest::anova(result.lmerTest, ddf="Kenward-Roger")

# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
# Note that the lmerTest package also has a lsmeans() function, so you need
# to fully specify the location of the function in the lsmeans package.
result.lmerTest.lsmo <- lsmeans::lsmeans(result.lmerTest, ~salt, adjust='tukey')  

# The rest continues in the same way as before and is not shown in detail here, e.g.
summary(result.lmerTest.lsmo, infer=TRUE)


#------------------------------- Unbalanced design (missing values -----------
# do a random sample of the data
set.seed(23423) # always set the seed to make your results reproducible

marsh2 <- marsh[ sample(1:nrow(marsh), .60*nrow(marsh), replace=FALSE),]

cat("*** Make an unbalanced design. It is still connected which is good \n")
xtabs(~block+salt, data=marsh2)

# Compare the anova from the different orders of the model fit
# Because R computes incremental SS, you should always put blocking variables
# first. In more complex designs that are unbalanced, you need the TYpe III SS
# computed by the Anova() function from the 'car' package
anova(lm(biomass ~ block + salt, data=marsh2))
anova(lm(biomass ~ salt + block, data=marsh2))

# Fit the linear model, get the anova table, and the usual stuff
# CAUTION!!! Because the design is unbalance, the default model
# fit by aov gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html
#
# You can also use the Anova() function from the car package.
cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat(  "\nBEFORE fitting the lm() model!")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")

# Get the TYpe III tests.
library(car)
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly")) 
options()$contrasts
Anova(lm(biomass ~ block + salt, data=marsh2), type=3)
Anova(lm(biomass ~ salt + block, data=marsh2), type=3)
options(old.options)


# The above tests only use the intra-block information.
# If you fit a random effects for blocks, you get both the
# intra- and inter-block analyses.
result.ubal.lmerTest <- lmer(biomass ~ salt + (1|block), data=marsh2)
lmerTest::anova(result.ubal.lmerTest)

# Similarly, look at the SE of the marginal means and of the pairwise 
# comparisons
result.ubal.lmerTest.lsmo <- lsmeans::lsmeans(result.ubal.lmerTest, ~salt, adjust='tukey')  

# The rest continues in the same way as before and is not shown in detail here, e.g.
summary(result.ubal.lmerTest.lsmo, infer=TRUE)
cld(result.ubal.lmerTest.lsmo, adjust='tukey')

pairs(result.ubal.lmerTest.lsmo, adjust='tukey')



lsmeans::lsmeans( lm(biomass ~ block + salt, data=marsh2), pairwise~salt, adjust='tukey')
lsmeans::lsmeans(lme4::lmer(biomass ~ salt + (1|block), data=marsh2), pairwise~salt, adjust='tukey')
