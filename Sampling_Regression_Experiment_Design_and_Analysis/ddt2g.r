# Compare ddt in male and female birds
# 2015-04-16 CJS fixed ##--- problem; no Base R graphics
# 2015-02-27 CJS scrap Base R graphics; scrap doBy package; use url for functions
# 2014-04-21 CJS ggplot, lsmeans, etc
#
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

#source('../../schwarz.functions.r') # in case no internet available
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


# Read in the data
sink('ddt2g-R-001.txt', split=TRUE)
##***part001b;
birds <- read.csv('ddt.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
birds$sex <- factor(birds$sex) # create the factor
birds
##***part001e;
sink()

##***part002b;
# Get side-by-side dot plot
plotdot <- ggplot(data=birds, aes(x=sex, y=ddt))+
     ggtitle("DDT readings broken out by sex")+
     xlab("Sex of birds")+ylab("DDT readings")+
     geom_point(position=position_jitter(w=0.1))+
     geom_boxplot(alpha=0.2, width=0.2)
plotdot
##***part002e;
ggsave(plot=plotdot, file="ddt2g-R-002.png",
   h=4, w=6, units="in", dpi=300)  # send the plot to a png file


sink('ddt2g-R-003.txt', split=TRUE)
##***part003b;
# Compute some summary statistics using ddply and summarize
report <- ddply(birds, "sex", summarize,
       n.birds=length(ddt),
       mean.ddt = mean(ddt),
       sd.ddt   = sd(ddt))
report
# Compute some summary statistics by sex using my helper function
report <- ddply(birds, "sex", sf.simple.summary, variable="ddt", crd=TRUE)
report
##***part003e;
sink()


# get the individual confidence intervals for each sex
# using a plyr package paradigm. This is what is done in the sf.simple.summary() function.
sink('ddt2g-R-004.txt', split=TRUE)
##***part004b;
ci <- ddply(birds, "sex", function(x){
   # compute the ci for the part of the dataframe x
   oneci <- t.test(x$ddt)$conf.int
   names(oneci) <- c("lcl","ucl")
   return(oneci)
})
ci
##***part004e;
sink()


# do the two sample t-test to see if the population mean ddt is
# the same in both sexes not assuming equal variances
sink('ddt2g-R-005.txt', split=TRUE)
##***part005b;
result <- t.test(ddt ~ sex, data=birds)
# compute the se
names(result)
result$se <- sum(result$estimate*c(1,-1)) /result$statistic
names(result$se)<- "SE.diff"
result
result$se
##***part005e;
sink()


# do the two sample t-test  assuming equal variances
# This is the same as a single factor CRD ANOVA on the data
sink('ddt2g-R-006.txt', split=TRUE)
##***part006b;
result <- t.test(ddt ~ sex, data=birds, var.equal=TRUE)
result$se <- sum(result$estimate*c(1,-1)) /result$statistic 
names(result$se)<- "SE.diff"
result
result$se
##***part006e;
sink()

#-------------------------------------------------------------
# Illustration of a single factor CRD ANOVA on this data set
# This is overkill, but illustrates the methods

# Make sure that sex is declared as a factor (when read in)
str(birds)


# Get side-by-side dot and box plots
plotprelim <- ggplot(birds, aes(x=sex, y=ddt))+
     ggtitle("ddt of both sexs")+
     xlab("sex")+
     ylab("ddt")+
     geom_point(position=position_jitter(height=0.1, width=0.1))+
     geom_boxplot(alpha=0.2, notch=TRUE)
plotprelim

ggsave(plotprelim, file="ddt2g-R-prelim.png", h=4, w=6, unit="in", dpi=300)


# Compute some summary statistics for each sex 

library(plyr)
report <- ddply(birds, "sex", sf.simple.summary, variable="ddt", crd=TRUE)
report


# fit the linear model and get the ANOVA table and test for effects
result <- lm(ddt ~ sex, data=birds)
anova(result)

# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag


# Now to estimate the marginal means and do a multiple comparison procedure
result.lsmo <- lsmeans(result, ~sex, adjust='tukey')

# Get the individual means
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)

cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld

# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="sex")
plotcld <- plotcld + 
        xlab("sex")+
        ylab("Mean ddt (with 95% ci")+
        ggtitle("Comparison of mean ddt with cld")
plotcld
#ggsave(plot=plotcld, file='ddt2g-R-cldbar.png',h=4, w=6, units='in', dpi=300)


# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="sex")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("sex")+
        ylab("Mean ddt (with 95% ci")+
        ggtitle("Comparison of mean ddt with cld")
plotcldb
#ggsave(plot=plotcldb, file='ddt2g-R-cldline.png', h=4, w=6, units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

# Find all the pairwise differences adjusting for multipicity
result.pairs <- pairs(result.lsmo, adjust='tukey')
summary(result.pairs, infer=TRUE)

# Make a plot of the differences
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
#ggsave(plot=plotdiff, file='ddt2g-R-pairdiff.png', h=4, w=6, unit="in", dpi=300)

