# Yields of barley
# 2014-04-20 CJS ggplot, lsmeans, etc

# The yields of two varieties of barley were compared 
# in a paired experimental design.

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
sink('paired-barley-R-001.txt', split=TRUE)
##---part001b;
fields <- read.csv('paired-barley.csv', header=TRUE, as.is=TRUE)
fields
##---part001e;
sink()

##---part001ab;
# Do a plot to see if there are any outliers
plot001 <- ggplot(data=fields, aes(x=V1, y=V2))+
    ggtitle("Check for outliers")+
    xlab("Variety 1")+ylab("Variety 2")+
    geom_point(size=4)
plot001
##---part001ae;

ggsave(plot=plot001, file='paired-barley-R-001a.png')

# Similar plot to above using Base R graphics
plot(fields$V1, fields$V2,
     main="Check for outliers",
     xlab="Variety 1", ylab="Variety 2")


# Fix the outlier
##---part001bb;
fields[fields$Farm==2,"V1"]<- 333
##---part001be;
fields

plot001b <- ggplot(data=fields, aes(x=V1, y=V2))+
    ggtitle("Check for outliers - after correcting the outlier")+
    xlab("Variety 1")+ylab("Variety 2")+
    geom_point(size=4)
plot001b

plot(fields$V1, fields$V2,
     main="Check for outliers - after correcting the outlier",
     xlab="Variety 1", ylab="Variety 2")


#*************************************************************************
# Analyze the differences

# Compute the difference between the densities

sink('paired-barley-R-003.txt', split=TRUE)
##---part003b;
fields$diff <- fields$V1 - fields$V2
fields
##---part003e;
sink()

# Get dot plot to see if any outliers etc
##---part004b;
plot004 <- ggplot(data=fields, aes(x="Difference", y=diff))+
  geom_jitter(position=position_jitter(w=0.05))+
  geom_hline(yintercept=0)+
  #geom_boxplot(notch=TRUE, alpha=0.2)+
  ggtitle("Plot of differences in yield")+
  ylab("Difference in yield")
plot004
##---part004e;

ggsave(plot=plot004, file='paired-barley-R-004.png')


# Get dot plot to see if any outliers etc using Base R graphics
stripchart(fields$diff,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Difference (V1-V2) of yield", 
   xlab='', ylab='Diff yield (V1-V2)')
abline(h=0, lty=2)


# Do the formal test and find the se by "hand"

sink("paired-barley-R-005.txt", split=TRUE)
##---part005b;
result <- t.test(fields$diff, alternative="less")
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
             ymin=min(fields$diff), ymax=result$conf.int[2], width=0.2)+
    ylab("Difference in yield with one sided confidence interval")
plot006

ggsave(plot=plot006, file="paired-barley-R-006.png")  


# Add the confidence interval to the dot plot using Base $ graphics
stripchart(fields$diff, add=FALSE, 
     vertical=TRUE, method="jitter", jitter=.1)
     abline(h=0, lty=2)
abline(h=result$estimate, lty=3, lwd=3)
# Note that because this is a one-sided ci, the lower bound is -Infinity
# We replace it by the lowerbound on the graph
segments(.9, min(fields$diff), .9,result$conf.int[2],
    lty=2, lwd=3)



#*************************************************************************
# Analyze the two columns directly


sink("paired-barley-R-010.txt",split=TRUE)
##---part010b;
result <- t.test(fields$V1, 
                 fields$V2, 
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
fields <- read.csv('paired-barley.csv', header=TRUE, as.is=TRUE)
fields[fields$Farm==2,"V1"]<- 333

# We need to stack the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
sink('paired-barley-R-019.txt', split=TRUE)
##---part019b;
fields2 <- reshape(fields, idvar="Field",
            varying=list(c("V1","V2")), 
            times=c("V1","V2"), timevar="variety",
            v.name="yield", 
            direction="long")
fields2
##---part019e;
sink()


# declare factor and blocking variables are factors
sink('paired-barley-R-020.txt', split=TRUE)
##---part020b;
fields2$variety  <- factor(fields2$variety)
fields2$Field    <- factor(fields2$Field)
str(fields2)
##---part020e;
sink()

# Check for block completeness
sink('paired-barley-R-checkcomplete.txt', split=TRUE)
##---partcheckcompleteb;
xtabs(~Field+variety, data=fields2)
##---partcheckcompletee;
sink()



# Compute some summary statistics for each group
sink('paired-barley-R-021.txt', split=TRUE)
##---part021b;
report <- ddply(fields2, "variety", sf.simple.summary, variable="yield")
report
##---part021e;
sink()

# Using the doBy package
report <- summaryBy( yield ~ variety, data=fields2, FUN=c(length,mean,sd))
# We don't compute a se here because the design is not a CRD
report


# fit the linear model and get the ANOVA table and test for effects
# Be sure that both variables are defined as FACTORS by R.
# Be sure to put the blocking variable first in the model
sink('paired-barley-R-022.txt', split=TRUE)
##---part022b;
result <- lm(yield ~ Field + variety, data=fields2)
anova(result)
##---part022e;
sink()


##---partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##---partdiage;

ggsave(plot=plotdiag, file='paired-barley-R-diag.png')

# Check the assumptions of the ANOVA model using Base R graphics
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


sink('paired-barley-R-lsmeansreport.txt', split=TRUE)
##---partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~variety, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##---partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('paired-barley-R-cldreport.txt', split=TRUE)
##---partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##---partcldreporte;
sink()

##---partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="variety")
plotcld <- plotcld + 
        xlab("variety")+
        ylab("Mean yield (with 95% ci)")+
        ggtitle("Comparison of mean yield with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="variety")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("variety")+
        ylab("Mean yield (with 95% ci)")+
        ggtitle("Comparison of mean yield with cld")
plotcldb
##---partcldplotse;

ggsave(plot=plotcld,  file='paired-barley-R-cldbar.png')
ggsave(plot=plotcldb, file='paired-barley-R-cldline.png')


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('paired-barley-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='paired-barley-R-pairdiff.png')


# Same plot using the glht package and the default plotting methods
result.pairs.glht <- as.glht(result.pairs)
result.pairs.glht.ci <-confint(result.pairs.glht) # extract the confint
result.pairs.glht.ci$confint

old.par <- par(mar=c(5,9,4,2)) # adjust the left margin of th eplot
plot(result.pairs.glht)# 
par(old.par)

