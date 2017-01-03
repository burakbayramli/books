# Compare Turbidity at several sites on French Creek.
# 2014-04-20 CJS ggplot, lsmeans etc

# French Creek was monitored for several months at two sites (Barclay Bride and Coombs).
# At the synpotic times, several water quality variables were measured, including
# Turbidity in NTU.
#
# We will compare the Turbidity between Barclay Bridge and Coombs
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
sink('TurbidityCompareBBvsCoombs-R-001.txt', split=TRUE)
##---part001b;
wq <- read.csv('TurbidityCompare.csv', header=TRUE)
# Discard all but the Barclay Bridge and Coombs readings
wq <- wq[,1:3]
print(wq)
##---part001e;
sink()



#*************************************************************************
# Analyze the log(ratio) of the two readings

# Compute the log(ratio) of the Barclay Bridge and Coombs readings

sink('TurbidityCompareBBvsCoombs-R-003.txt', split=TRUE)
##---part003b;
wq$logratio <- log(wq$BB/wq$Coombs)
wq[1:6,]
##---part003e;
sink()

# Get dot plot to see if any outliers etc
##---part004b;
plot004 <- ggplot(data=wq, aes(x="Difference", y=logratio))+
  geom_jitter(position=position_jitter(w=0.05))+
  geom_hline(yintercept=0)+
  #geom_boxplot(notch=TRUE, alpha=0.2)+
  ggtitle("Plot of log(ratio) of turbidity")+
  ylab("log(ratio - BB/Coombs - turbidity")
plot004
##---part004e;

ggsave(plot=plot004, file='TurbidityCompareBBvsCoombs-R-004.png')

# Dot plut using Base R graphics
stripchart(wq$logratio,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Log ratio (BB/Coombs) NTU Units", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Log ratio (BB/Coombsh)')



# Do the formal test and find the se by "hand"
sink("TurbidityCompareBBvsCoombs-R-005.txt", split=TRUE)
##---part005b;
result <- t.test(wq$logratio, )
names(result)
result$se.meanlogratio <- result$estimate / result$statistic
# Convert back to ratio on the anti-log scale
result$ratio   <- exp(result$estimate)
result$se.ratio<- result$se.meanlogratio * result$ratio
print(result)
cat("Estimated mean log-ratio is ", result$estimate," (SE ", result$se.meanlogratio, ")\n")
cat("Estimate median ratio (BB/Coombs) is", result$ratio," (SE ", result$se.ratio, ")\n")
##---part005e;
sink()


##---part006b;
# Add the confidence interval to the dot plot
plot006 <- plot004 + 
    annotate("point", x="Difference", y=result$estimate, size=6, shape=2)+
    annotate("errorbar", x="Difference", 
             ymin=result$conf.int[1], ymax=result$conf.int[2], width=0.2)+
    ylab("Log(ratio) with confidence interval")
plot006
##---part0063;

ggsave(plot=plot006, file="TurbidityCompareBBvsCoombs-R-006.png")  

# Repeat for Base R graphics
stripchart(wq$logratio, add=FALSE, 
     vertical=TRUE, method="jitter", jitter=.1)
     abline(h=0, lty=2)
abline(h=result$estimate, lty=3, lwd=3)
segments(.9, result$conf.int[1], .9,result$conf.int[2], lty=2, lwd=3)


#*************************************************************************
# Analyze the two columns directly

# We need to log both readings to compare the log(ratio)

sink("TurbidityCompareBBvsCoombs-R-010.txt", split=TRUE)
##---part010b;
wq$logBB     <- log(wq$BB)
wq$logCoombs <- log(wq$Coombs)

result <- t.test(wq$logBB, 
                 wq$logCoombs, 
                 paired=TRUE)
names(result)
result$se.meanlogratio <- result$estimate / result$statistic# Convert back to ratio on the anti-log scale
result$ratio   <- exp(result$estimate)
result$se.ratio<- result$se.meanlogratio * result$ratio
print(result)

cat("Estimated mean log-ratio is ", result$estimate," (SE ", result$se.meanlogratio, ")\n")
cat("Estimate median ratio (BB/Coombs) is", result$ratio," (SE ", result$se.ratio, ")\n")
##---part010e;
sink()


#*************************************************************************
# Analyze as a linear model using aov() and lm()

# We first need to stack the BB and Coombs readings

sink('TurbidityCompareBBvsCoombs-R-019.txt', split=TRUE)
##---part019b;
wq2 <- melt(wq, 
                id.vars="SampleTime",
                measure.vars=c("BB","Coombs"),
                variable.name="Site", , 
                value.name="Turbidity")
wq2$logTurbidity <- log(wq2$Turbidity)
wq2[ c(1:3, 16:18),]
##---part019e;
sink()


# declare factor and blocking variables are factors
sink('TurbidityCompareBBvsCoombs-R-020.txt', split=TRUE)
##---part020b;
wq2$Site        <- factor(wq2$Site)
wq2$SampleTime  <- factor(wq2$SampleTime)
str(wq2)
##---part020e;
sink()

# Check for block completeness
sink('TurbidityCompareBBvsCoombs-R-checkcomplete.txt', split=TRUE)
##---partcheckcompleteb;
xtabs(~SampleTime+Site, data=wq2)
##---partcheckcompletee;
sink()



sink('TurbidityCompareBBvsCoombs-R-021.txt', split=TRUE)
##---part021b;
# Compute some summary statistics for each group
# We don't compute a se here because the design is not a CRD
report <- ddply(wq2, "Site", sf.simple.summary, variable="logTurbidity")
report
##---part021e;
sink()

# Using the doBy package
# We don't compute a se here because the design is not a CRD
report <- summaryBy( logTurbidity ~ Site, data=wq2, FUN=c(length,mean,sd))
report


# fit the linear model and get the ANOVA table and test for effects
# Be sure that both variables are defined as FACTORS by R.
# Be sure to put the blocking variable first in the model
sink('TurbidityCompareBBvsCoombs-R-022.txt', split=TRUE)
##---part022b;
result <- lm(logTurbidity ~ SampleTime + Site, data=wq2)
anova(result)
##---part022e;
sink()


##---partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##---partdiage;

ggsave(plot=plotdiag, file='TurbidityCompareBBvsCoombs-R-diag.png')

# Check the assumptions of the ANOVA model using Base R graphics
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


sink('TurbidityCompareBBvsCoombs-R-lsmeansreport.txt', split=TRUE)
##---partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans(result, ~Site, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##---partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('TurbidityCompareBBvsCoombs-R-cldreport.txt', split=TRUE)
##---partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##---partcldreporte;
sink()

##---partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="Site")
plotcld <- plotcld + 
        xlab("Site")+
        ylab("Mean logTurbidity (with 95% ci)")+
        ggtitle("Comparison of mean logTurbidity with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="Site")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("Site")+
        ylab("Mean logTurbidity (with 95% ci)")+
        ggtitle("Comparison of mean logTurbidity with cld")
plotcldb
##---partcldplotse;

ggsave(plot=plotcld,  file='TurbidityCompareBBvsCoombs-R-cldbar.png')
ggsave(plot=plotcldb, file='TurbidityCompareBBvsCoombs-R-cldline.png')


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('TurbidityCompareBBvsCoombs-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='TurbidityCompareBBvsCoombs-R-pairdiff.png')


# Same plot using the glht package and the default plotting methods
result.pairs.glht <- as.glht(result.pairs)
result.pairs.glht.ci <-confint(result.pairs.glht) # extract the confint
result.pairs.glht.ci$confint

old.par <- par(mar=c(5,9,4,2)) # adjust the left margin of th eplot
plot(result.pairs.glht)# 
par(old.par)


