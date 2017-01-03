# Compare Turbidity at Multiple Sites
# 2015-04-18 CJS misc changes
# 2014-04-20 CJS ggplot, split, lsmeans etc

# French Creek was monitored for several months at two sites (Barclay Bride and Coombs).
# At the synpotic times, several water quality variables were measured, including
# Turbidity in NTU.
#
# We will compare the Turbidity between Barclay Bridge and Coombs
#
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

#source('../../schwarz.functions.r') # if no internet available
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


sink('TurbidityCompareRCB-R-001.txt', split=TRUE)
##***part001b;
turbidity <- read.csv('TurbidityCompare.csv', header=TRUE, as.is=TRUE)
turbidity
##***part001e;
sink()

# We need to reshape the data with a column for the sample time, one column 
# for the site, and one column for log(turbidity)
# Also remove any missing values
sink('TurbidityCompareRCB-R-019.txt', split=TRUE)
##***part019b;
turbidity_melt <- melt(turbidity, 
      id.vars=c("SampleTime"), 
      measure.vars=c("BB","Coombs","Grafton","NewHwy","WinchRd"),
      variable.name="Site",
      value.name="Turbidity")
turbidity_melt$logTurbidity <- log(turbidity_melt$Turbidity)
turbidity_melt<- turbidity_melt[!is.na(turbidity_melt$logTurbidity),]
print(turbidity_melt[1:10,])
##***part019e;
sink()

# Declare both block and trt variable  as factors
sink('TurbidityCompareRCB-R-020.txt', split=TRUE)
##***part020b;
turbidity_melt$Site       <- factor(turbidity_melt$Site)
turbidity_melt$SampleTime <- factor(turbidity_melt$SampleTime)
str(turbidity_melt)
##***part020e;
sink()


##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=turbidity_melt, aes(x=Site, y=logTurbidity))+
  ggtitle("log(Turbidity) at various sites")+
  xlab("Site")+ylab("log(Turbidity) - log(NTU)")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='TurbidityCompareRCB-R-prelim.png', h=4, w=6, units="in", dpi=300)



# Profile plot
##***partprofileplotb;
# Side-by-side dot and boxplots 
plotprofile <- ggplot(data=turbidity_melt, 
                      aes(x=SampleTime, y=logTurbidity, group=Site, shape=Site))+
  ggtitle("log(Turbidity) over time")+
  xlab("Samplt Time")+ylab("log(Turbidity) - log(NTU)")+
  geom_point(size=4)+
  geom_line()
plotprofile
##***partprofileplote;

ggsave(plot=plotprofile, file='TurbidityCompareRCB-R-profile.png', h=4,w=6, units="in", dpi=300)

	

# Check for block completeness
sink('TurbidityCompareRCB-R-checkcomplete.txt', split=TRUE)
##***partcheckcompleteb;
xtabs(~SampleTime+Site, data=turbidity_melt)
##***partcheckcompletee;
sink()

# Compute some summary statistics for each group using doBy package
sink('TurbidityCompareRCB-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(turbidity_melt, "Site", sf.simple.summary, variable="logTurbidity")
report
##***part003e;
sink()



# fit the linear model and get the ANOVA table and test for effects
# Note that because the design is UNbalanced (some missing data), we must fit the block term 
# first and then the treatment term -- R gives the "Type I" ss otherwise

sink('TurbidityCompareRCB-R-030.txt', split=TRUE)
##***part030b;
result <- lm(logTurbidity ~ SampleTime + Site, data=turbidity_melt)
anova(result)
##***part030e;
sink()



##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='TurbidityCompareRCB-R-diag.png', h=6, w=6, units="in", dpi=300)




sink('TurbidityCompareRCB-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans::lsmeans(result, ~Site, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()


cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('TurbidityCompareRCB-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="Site")
plotcld <- plotcld + 
        xlab("Site")+
        ylab("Mean log(Turbidity) (with 95% ci)")+
        ggtitle("Comparison of mean log(Turbidity) with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="Site")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("Site")+
        ylab("Mean log(Turbidity) (with 95% ci)")+
        ggtitle("Comparison of mean log(Turbidity) with cld")
plotcldb
##***partcldplotse;

ggsave(plot=plotcld, file='TurbidityCompareRCB-R-cldbar.png'  ,h=4, w=6,units="in", dpi=300)
ggsave(plot=plotcldb, file='TurbidityCompareRCB-R-cldline.png',h=4, w=6,units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('TurbidityCompareRCB-R-pairsreport.txt', split=TRUE)
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

ggsave(plot=plotdiff, file='TurbidityCompareRCB-R-pairdiff.png', h=4, w=6,units="in", dpi=300)

