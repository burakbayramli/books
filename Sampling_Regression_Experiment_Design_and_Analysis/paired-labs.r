# Inter-lab comparison
# 2014-04-20 CJS ggplot, lsmeans etc.

# An inter-lab comparison was made where different samples 
# were split into two. These were randomly sent to one of 
# two participating labs.

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
sink('paired-labs-R-001.txt', split=TRUE)
##---part001b;
labs <- read.csv('paired-labs.csv', header=TRUE, as.is=TRUE)
labs
##---part001e;
sink()

##---part001ab;
# Do a plot to see if there are any outliers
plot001 <- ggplot(data=labs, aes(x=lab1, y=lab2))+
    ggtitle("Check for outliers")+
    xlab("Lab 1")+ylab("Lab 2")+
    geom_point(size=4)
plot001
##---part001ae;

ggsave(plot=plot001, file='paired-labs-R-001a.png')

# Similar plot to above using Base R graphics
plot(labs$lab1, labs$lab2,
     main="Check for outliers",
     xlab="Lab 1", ylab="Lab 2")




#*************************************************************************
# Analyze the differences

# Compute the difference between the densities

sink('paired-labs-R-003.txt', split=TRUE)
##---part003b;
labs$diff <- labs$lab1 - labs$lab2
labs
##---part003e;
sink()

# Get dot plot to see if any outliers etc
##---part004b;
plot004 <- ggplot(data=labs, aes(x="Difference", y=diff))+
  geom_jitter(position=position_jitter(w=0.05))+
  geom_hline(yintercept=0)+
  #geom_boxplot(notch=TRUE, alpha=0.2)+
  ggtitle("Plot of differences in reading")+
  ylab("Difference in reading")
plot004
##---part004e;

ggsave(plot=plot004, file='paired-labs-R-004.png')


# Get dot plot to see if any outliers etc using Base R graphics
stripchart(labs$diff,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Difference (lab1-lab2) of reading", 
   xlab='', ylab='Diff reading (lab1-lab2)')
abline(h=0, lty=2)


# Do the formal test and find the se by "hand"

sink("paired-labs-R-005.txt", split=TRUE)
##---part005b;
result <- t.test(labs$diff)
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
             ymin=result$conf.int[1], ymax=result$conf.int[2], width=0.2)+
    ylab("Difference in readings with one sided confidence interval")
plot006

ggsave(plot=plot006, file="paired-labs-R-006.png")  


# Add the confidence interval to the dot plot using Base $ graphics
stripchart(labs$diff, add=FALSE, 
     vertical=TRUE, method="jitter", jitter=.1)
     abline(h=0, lty=2)
abline(h=result$estimate, lty=3, lwd=3)
# Note that because this is a one-sided ci, the lower bound is -Infinity
# We replace it by the lowerbound on the graph
segments(.9, min(labs$diff), .9,result$conf.int[2], lty=2, lwd=3)



#*************************************************************************
# Analyze the two columns directly


sink("paired-labs-R-010.txt",split=TRUE)
##---part010b;
result <- t.test(labs$lab1, 
                 labs$lab2, 
                 paired=TRUE)
names(result)
result$se.diff <- result$estimate / result$statistic
result
cat("SE of the diff in means is ", result$se.diff, "\n")
##---part010e;
sink()


#*************************************************************************
# Analyze as a linear model using  lm()
labs <- read.csv('paired-labs.csv', header=TRUE, as.is=TRUE)

# We need to stack the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
sink('paired-labs-R-019.txt', split=TRUE)
##---part019b;
labs2 <- reshape(labs, idvar="Lab",
            varying=list(c("lab1","lab2")), 
            times=c("lab1","lab2"), timevar="lab",
            v.name="reading", 
            direction="long")
labs2
##---part019e;
sink()


# declare factor and blocking variables are factors
sink('paired-labs-R-020.txt', split=TRUE)
##---part020b;
labs2$lab     <- factor(labs2$lab)
labs2$Sample  <- factor(labs2$Sample)
str(labs2)
##---part020e;
sink()

# Check for block completeness
sink('paired-labs-R-checkcomplete.txt', split=TRUE)
##---partcheckcompleteb;
xtabs(~Sample+lab, data=labs2)
##---partcheckcompletee;
sink()




sink('paired-labs-R-021.txt', split=TRUE)
##---part021b;
# Compute some summary statistics for each group
# We don't compute a se here because the design is not a CRD
report <- ddply(labs2, "lab", sf.simple.summary, variable="reading")
report
##---part021e;
sink()

# Using the doBy package
# We don't compute a se here because the design is not a CRD
report <- summaryBy( reading ~ lab, data=labs2, FUN=c(length,mean,sd))
report


# fit the linear model and get the ANOVA table and test for effects
# Be sure that both variables are defined as FACTORS by R.
# Be sure to put the blocking variable first in the model
sink('paired-labs-R-022.txt', split=TRUE)
##---part022b;
result <- lm(reading ~ Sample + lab, data=labs2)
anova(result)
##---part022e;
sink()


##---partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##---partdiage;

ggsave(plot=plotdiag, file='paired-labs-R-diag.png')

# Check the assumptions of the ANOVA model using Base R graphics
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


sink('paired-labs-R-lsmeansreport.txt', split=TRUE)
##---partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~lab, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##---partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('paired-labs-R-cldreport.txt', split=TRUE)
##---partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##---partcldreporte;
sink()

##---partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="lab")
plotcld <- plotcld + 
        xlab("lab")+
        ylab("Mean reading (with 95% ci)")+
        ggtitle("Comparison of mean reading with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="lab")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("lab")+
        ylab("Mean reading (with 95% ci)")+
        ggtitle("Comparison of mean reading with cld")
plotcldb
##---partcldplotse;

ggsave(plot=plotcld,  file='paired-labs-R-cldbar.png')
ggsave(plot=plotcldb, file='paired-labs-R-cldline.png')


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('paired-labs-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='paired-labs-R-pairdiff.png')


# Same plot using the glht package and the default plotting methods
result.pairs.glht <- as.glht(result.pairs)
result.pairs.glht.ci <-confint(result.pairs.glht) # extract the confint
result.pairs.glht.ci$confint

old.par <- par(mar=c(5,9,4,2)) # adjust the left margin of th eplot
plot(result.pairs.glht)# 
par(old.par)



