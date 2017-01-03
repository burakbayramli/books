# RCB 

# Blood Pressure at presyncope under different experimental conditions.
# 2014-04-20 CJS ggplot, lsmeans, etc

# 15 subjects were measured while wearing three different stocking to control
# presyncope. The order of treatments was randomized within each patient.

#   Data provided by Clare Protheroe of BPK at SFU in 2011 */

#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusing by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

# Load the necessary libraries
library(doBy)
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)

source('../../schwarz.functions.r')

options(width=200)



sink('BPatPresyncope-R-000.txt', split=TRUE)
# Read in the data
##---part000b;
BP <- read.csv('BPatPresyncope.csv', header=TRUE, as.is=TRUE)
print(BP[1:12,])
##---part000e;
sink()


# Declare both block and trt variable  as factors
sink('BPatPresyncope-R-020.txt', split=TRUE)
##---part020b;
BP$treatment  <- factor(BP$treatment)
BP$subject    <- factor(BP$subject)
str(BP)
##---part020e;
sink()


##---partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=BP, aes(x=treatment, y=bp_at_presyncope))+
  ggtitle("bp_at_presyncope at various treatments")+
  xlab("treatment")+ylab("bp_at_presyncope")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2)
plotprelim
##---partprelimplote;

ggsave(plot=plotprelim, file='BPatPresyncope-R-prelim.png')

# There is an obvious outlier - remove the outlier
##---part004b;
BP <- BP[ ! BP$bp_at_presyncop < 40,]
##---part004e;

# Get the side-by-side plots after the outlier is removeed
plotprelim <- ggplot(data=BP, aes(x=treatment, y=bp_at_presyncope))+
  ggtitle("bp_at_presyncope at various treatments after outlier removed")+
  xlab("treatment")+ylab("bp_at_presyncope")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2)
plotprelim


# Get side-by-side dot plots using Base R graphics
boxplot( bp_at_presyncope ~ treatment, data=BP, range=0,
   notch=TRUE, # helps to compare pop means
   main="bp_at_presyncope at various treatments after outlier is removed", 
   sub='Whiskers extend to range of data',
   xlab='treatment', ylab='bp_at_presyncope (g)')

stripchart(bp_at_presyncope ~ treatment, data=BP, add=TRUE, 
     vertical=TRUE, method="jitter", jitter=.1)


# Profile plot
##---partprofileplotb;
# Side-by-side dot and boxplots 
plotprofile <- ggplot(data=BP, 
                      aes(x=subject, y=bp_at_presyncope, group=treatment, shape=treatment))+
  ggtitle("bp_at_presyncope over time after outlier removed")+
  xlab("subject")+ylab("bp_at_presyncope")+
  geom_point(size=4)+
  geom_line()
plotprofile
##---partprofileplote;

ggsave(plot=plotprofile, file='BPatPresyncope-R-profile.png')


# Profile plot using Base R graphics
# Do a plot of the bp_at_presyncope over the treatments by sample to check for additivity
stripchart(bp_at_presyncope ~ treatment, data=BP, vertical=TRUE)
d_ply(BP,"subject", 
    function(x){points(as.numeric(x$treatment), x$bp_at_presyncope, type="l")})
   

# Check for block completeness
sink('BPatPresyncope-R-checkcomplete.txt', split=TRUE)
##---partcheckcompleteb;
xtabs(~subject+treatment, data=BP)
##---partcheckcompletee;
sink()






# Compute some summary statistics for each group 
# We don't compute a se here because the design is not a CRD
sink('BPatPresyncope-R-003.txt', split=TRUE)
##---part003b;
report <- ddply(BP, "treatment", sf.simple.summary, variable="bp_at_presyncope")
report
##---part003e;
sink()

# We don't compute a se here because the design is not a CRD
report <- summaryBy( bp_at_presyncope ~ treatment, data=BP, FUN=c(length,mean,sd))
report




# fit the linear model and get the ANOVA table and test for effects
# Note that because the design is UNbalanced (some missing data), we must fit the block term 
# first and then the treatment term -- R gives the "Type I" ss otherwise

sink('BPatPresyncope-R-030.txt', split=TRUE)
##---part030b;
result <- lm(bp_at_presyncope ~ subject + treatment, data=BP)
anova(result)
##---part030e;
sink()



##---partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##---partdiage;

ggsave(plot=plotdiag, file='BPatPresyncope-R-diag.png')

# Check the assumptions of the ANOVA model using Base R graphics
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


sink('BPatPresyncope-R-lsmeansreport.txt', split=TRUE)
##---partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~treatment, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##---partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('BPatPresyncope-R-cldreport.txt', split=TRUE)
##---partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##---partcldreporte;
sink()

##---partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="treatment")
plotcld <- plotcld + 
        xlab("treatment")+
        ylab("Mean bp_at_presyncope (with 95% ci)")+
        ggtitle("Comparison of mean bp_at_presyncope with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="treatment")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("treatment")+
        ylab("Mean bp_at_presyncope (with 95% ci)")+
        ggtitle("Comparison of mean bp_at_presyncope with cld")
plotcldb
##---partcldplotse;

ggsave(plot=plotcld, file='BPatPresyncope-R-cldbar.png')
ggsave(plot=plotcldb, file='BPatPresyncope-R-cldline.png')


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('BPatPresyncope-R-pairsreport.txt', split=TRUE)
##---partpairsb;
# Find all the pairwise differences adjusting for multipicity
result.pairs <- pairs(result.lsmo, adjust='tukey')
summary(result.pairs, infer=TRUE)
##---partpairse;
sink()


# Make a plot of the differences
##---partpairsplotb;
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
##---partpairsplote;

ggsave(plot=plotdiff, file='BPatPresyncope-R-pairdiff.png')


# Same plot using the glht package and the default plotting methods
result.pairs.glht <- as.glht(result.pairs)
result.pairs.glht.ci <-confint(result.pairs.glht) # extract the confint
result.pairs.glht.ci$confint

old.par <- par(mar=c(5,9,4,2)) # adjust the left margin of th eplot
plot(result.pairs.glht)# 
par(old.par)


#--------------------------------------------------------------------------------------
# Extract the simulaneous inference using the base R function TukeyHSD
# This requires an aov() fit
result <- aov(bp_at_presyncope ~ subject + treatment, data=BP)
mcp <- TukeyHSD(result, which="treatment", ordered=TRUE) # ordered sorts means
mcp
plot(mcp)
abline(v=0, lty=2)

# Using the multcomp package
library(multcomp)
result.tukey     <- glht(result, linfct = mcp(treatment = "Tukey"))
result.tukey.cld <- cld(result.tukey)  # joined line plot

# create the display
# Notice that the plot() function applied to a cld object (the plot.cld() method)
# DOES NOT produce a letter if the treatment factor label contains a blank!
# FOr example, in the plot below, notice that there is NO letter above the
# treatment "24D TCA" because it contains a blank.
result.tukey.cld
old.par <- par(mai=c(1.25,1.25,1.50,1))
plot(result.tukey.cld, 
     xlab="treatment",
     ylab="bp_at_presyncope", 
     notch=TRUE)
title(main="Multiple comparison results", line=1)
par <- par(old.par)



# We compare the results to a model with random subjects. The results
# will be very similar because the design is nearly balanced and only
# has one missing value. The results would be identical if the design was complete.
library(lmerTest)  # you must load in the proper order (R is free, but not cheap)
result.lmerTest <- lmerTest::lmer(bp_at_presyncope ~ treatment + (1 | subject), data=BP)
lmerTest::anova(result.lmerTest, ddf="Kenward-Roger")

# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
# Note that the lmerTest package also has a lsmeans() function, so you need
# to fully specify the location of the function in the lsmeans package.
result.lmerTest.lsmo <- lsmeans::lsmeans(result.lmerTest, ~treatment, adjust='tukey')  

# The rest continues in the same way as before and is not shown in detail here, e.g.
summary(result.lmerTest.lsmo, infer=TRUE)
cld(result.lmerTest.lsmo)
pairs(result.lmerTest.lsmo)




# Now for the power analysis
# From the previous analyses, the 
#   standard deviation is around 9 mm HG
#   the expected means are 65, 65, 70
#   the required power is .80
#   the significance level = 0.05
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.


sink('BPatPresyncope-R-050.txt', split=TRUE)
##---part050b;
group.means <- c(65, 65, 70)
power <- power.anova.test(groups=length(group.means), 
         within.var=9**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##---part050e;
sink()







