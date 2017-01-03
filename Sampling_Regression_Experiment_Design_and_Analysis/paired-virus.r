# Comparing two preparations of tobacco mosaic virus.
# 2014-04-20 CJS ggplot, lsmeans etc.

# Two different preparations of tobacco mosaic virus
# were compared by painting it on two leaves of each
# plant and counting the number
# of subsequent virus spots on each leaf.


#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
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


# Read in the data
sink('paired-virus-R-001.txt', split=TRUE)
##---part001b;
plants <- read.csv('paired-virus.csv', header=TRUE)
plants
##---part001e;
sink()


##---part001ab;
# Do a plot to see if there are any outliers
plot001 <- ggplot(data=plants, aes(x=Prep1, y=Prep2))+
    ggtitle("Check for outliers")+
    xlab("Prep 1")+ylab("Prep 2")+
    geom_point(size=4)
plot001
##---part001ae;

ggsave(plot=plot001, file='paired-virus-R-001a.png')

# Similar plot to above using Base R graphics
plot(plants$Prep1, plants$Prep2,
     main="Check for outliers",
     xlab="Prep 1", ylab="Prep 2")




#*************************************************************************
# Analyze the differences

# Compute the difference between the densities

sink('paired-virus-R-003.txt', split=TRUE)
##---part003b;
plants$diff <- plants$Prep1 - plants$Prep2
plants
##---part003e;
sink()

# Get dot plot to see if any outliers etc
##---part004b;
plot004 <- ggplot(data=plants, aes(x="Difference", y=diff))+
  geom_jitter(position=position_jitter(w=0.05))+
  geom_hline(yintercept=0)+
  #geom_boxplot(notch=TRUE, alpha=0.2)+
  ggtitle("Plot of differences in lesions")+
  ylab("Difference in lesions")
plot004
##---part004e;

ggsave(plot=plot004, file='paired-virus-R-004.png')


# Get dot plot to see if any outliers etc using Base R graphics
stripchart(plants$diff,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Difference (Prep1-Prep2) of lesions", 
   xlab='', ylab='Diff lesions (Prep1-Prep2)')
abline(h=0, lty=2)


# Do the formal test and find the se by "hand"

sink("paired-virus-R-005.txt", split=TRUE)
##---part005b;
result <- t.test(plants$diff, alternative="less")
names(result)
result$se.diff <- result$estimate / result$statistic
result
cat("SE of the diff in means is ", result$se.diff, "\n")
##---part005e;
sink()

##---part006b;
plot006 <- plot004 + 
    annotate("point", x="Difference", y=result$estimate, size=6, shape=2)+
    annotate("errorbar", x="Difference", 
             ymin=min(plants$diff), ymax=result$conf.int[2], width=0.2)+
    ylab("Difference in lesions with one sided confidence interval")
plot006

ggsave(plot=plot006, file="paired-virus-R-006.png")  


# Add the confidence interval to the dot plot using Base $ graphics
stripchart(plants$diff, add=FALSE, 
     vertical=TRUE, method="jitter", jitter=.1)
     abline(h=0, lty=2)
abline(h=result$estimate, lty=3, lwd=3)
# Note that because this is a one-sided ci, the lower bound is -Infinity
# We replace it by the lowerbound on the graph
segments(.9, min(plants$diff), .9,result$conf.int[2], lty=2, lwd=3)



#*************************************************************************
# Analyze the two columns directly


sink("paired-virus-R-010.txt",split=TRUE)
##---part010b;
result <- t.test(plants$Prep1, 
                 plants$Prep2, 
                 paired=TRUE,
                 alternative='less')
names(result)
result$se.diff <- result$estimate / result$statistic
result
cat("SE of the diff in means is ", result$se.diff, "\n")
##---part010e;
sink()


#*************************************************************************
# Analyze as a linear model using  lm()
plants <- read.csv('paired-virus.csv', header=TRUE, as.is=TRUE)

# We need to stack the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
sink('paired-virus-R-019.txt', split=TRUE)
##---part019b;
plants2 <- reshape(plants, idvar="Plant",
            varying=list(c("Prep1","Prep2")), 
            times=c("Prep1","Prep2"), timevar="Prep",
            v.name="lesions", 
            direction="long")
plants2
##---part019e;
sink()


# declare factor and blocking variables are factors
sink('paired-virus-R-020.txt', split=TRUE)
##---part020b;
plants2$Prep  <- factor(plants2$Prep)
plants2$Plant <- factor(plants2$Plant)
str(plants2)
##---part020e;
sink()

# Check for block completeness
sink('paired-virus-R-checkcomplete.txt', split=TRUE)
##---partcheckcompleteb;
xtabs(~Plant+Prep, data=plants2)
##---partcheckcompletee;
sink()



# Compute some summary statistics for each group
# We don't compute a se here because the design is not a CRD
sink('paired-virus-R-021.txt', split=TRUE)
##---part021b;
report <- ddply(plants2, "Prep", sf.simple.summary, variable="lesions")
report
##---part021e;
sink()

# Using the doBy package
# We don't compute a se here because the design is not a CRD
report <- summaryBy( lesions ~ Prep, data=plants2, FUN=c(length,mean,sd))
report


# fit the linear model and get the ANOVA table and test for effects
# Be sure that both variables are defined as FACTORS by R.
# Be sure to put the blocking variable first in the model
sink('paired-virus-R-022.txt', split=TRUE)
##---part022b;
result <- lm(lesions ~ Plant + Prep, data=plants2)
anova(result)
##---part022e;
sink()


##---partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##---partdiage;

ggsave(plot=plotdiag, file='paired-virus-R-diag.png')

# Check the assumptions of the ANOVA model using Base R graphics
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


sink('paired-virus-R-lsmeansreport.txt', split=TRUE)
##---partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~Prep, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##---partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('paired-virus-R-cldreport.txt', split=TRUE)
##---partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##---partcldreporte;
sink()

##---partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="Prep")
plotcld <- plotcld + 
        xlab("Prep")+
        ylab("Mean lesions (with 95% ci)")+
        ggtitle("Comparison of mean lesions with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="Prep")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("Prep")+
        ylab("Mean lesions (with 95% ci)")+
        ggtitle("Comparison of mean lesions with cld")
plotcldb
##---partcldplotse;

ggsave(plot=plotcld,  file='paired-virus-R-cldbar.png')
ggsave(plot=plotcldb, file='paired-virus-R-cldline.png')


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('paired-virus-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='paired-virus-R-pairdiff.png')


# Same plot using the glht package and the default plotting methods
result.pairs.glht <- as.glht(result.pairs)
result.pairs.glht.ci <-confint(result.pairs.glht) # extract the confint
result.pairs.glht.ci$confint

old.par <- par(mar=c(5,9,4,2)) # adjust the left margin of th eplot
plot(result.pairs.glht)# 
par(old.par)

