# BACI-fish
# 2014-11-28 CJS sf.autoplot.lmer
# 2014-11-26 CJS ggplot, split=TRUE, lmer, ##*** problam

# This is the example that appears in Smith (2002, Table 6). 
# Samples of fish were taken for a period
# of 12 months before and 13 months after a nuclear
# power plant began operations. 

# The power plant is cooled by water that is drawn from a river.

# When the water exits the plant, its temperature is
# elevated. 
# The concern is that the warmed water will adversely affect the abundance 
# and composition of fish below the plant. 

# [It wasn't clear from Smith (2002) what the response measure is, 
# so I'll arbitrarily assume that it is counts.]

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lmerTest)
library(lsmeans)
library(plyr)

#source("../../schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

# Read in the actual data
sink("baci-fish-R-001.txt",split=TRUE)
##***part001b;
cat(" BACI design measuring fish counts with multiple (paired) yearly measurements before/after \n\n")

fish <- read.csv("baci-fish.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
fish$trt <- interaction(fish$SiteClass, fish$Site, fish$Period)
fish$Period        <- factor(fish$Period)
fish$SiteClass     <- factor(fish$SiteClass)
fish$Site          <- factor(fish$Site)
fish$SamplingTimeF <- factor(fish$SamplingTime)
fish$trt           <- factor(fish$trt)
cat("Listing of part of the raw data \n")
fish[1:10,]
##***part001e;
sink()

str(fish)

# Preliminary plot

# Get plot of series over time
##***part010b;
prelimplot <- ggplot(data=fish, aes(x=SamplingTime, y=Count,
                          group=Site, color=SiteClass, shape=Site))+
  ggtitle("Fish counts over time")+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=-0.5+min(fish$SamplingTime[as.character(fish$Period) == "After"]))
prelimplot
##***part010e;
ggsave(plot=prelimplot, file="baci-fish-R-010.png",
       h=4, w=6, units="in", dpi=300)



# There are several ways in which this BACI design can be analyzed.


###############################################################################
# Do a t-test on the differeces of the averages at each sampling time for each site
# There is only one observtion at each sampling time, so we don't have compute
# the averages here


sink('baci-fish-R-101.txt',split=TRUE)
##***part101b;
# Reshape the file to get the Control and Impact measurements on the same line
fish.month <- reshape(fish, v.names=c("Count"), timevar=c("SiteClass"), 
   idvar=c("SamplingTime","Period"), drop=c("Site","trt"), direction="wide" )
fish.month$diff <- fish.month$Count.Impact - fish.month$Count.Control
fish.month[1:10,]
##***part101e;
sink()


# Plot the difference over time
##***part102b;
plotdiff <- ggplot(data=fish.month, aes(x=SamplingTime, y=diff))+
  ggtitle("Plot of differences over time")+
  ylab("Difference (Impact-Control)")+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=-0.5+min(fish$SamplingTime[as.character(fish$Period) == "After"]))

plotdiff
##***part102e;
ggsave(plot=plotdiff, file="baci-fish-R-102.png", h=4, w=6, units="in", dpi=300)


# do the two sample t-test not assuming equal variances
sink('baci-fish-R-104.txt',split=TRUE)
##***part104b;
result <- try(t.test(diff ~ Period, data=fish.month),silent=TRUE)
if(class(result)=="try-error")
     {cat("Unable to do unequal variance t-test because of small sample size\n")} else
     { 
   result$diff.in.means <- sum(result$estimate*c(1,-1))
   names(result$diff.in.means)<- "diff.in.means"
   result$se.diff <- result$statistic / abs(result$diff.in.means)
   names(result$se.diff) <- 'SE.diff'
   print(result)
   print(result$diff.in.means)
   print(result$se.diff)
     }
##***part104e;
sink()


# do the two sample t-test  assuming equal variances
sink('baci-fish-R-105.txt',split=TRUE)
##***part105b;
result <- t.test(diff ~ Period, data=fish.month, var.equal=TRUE)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- result$statistic / abs(result$diff.in.means)
names(result$se.diff) <- 'SE.diff'
result
result$diff.in.means
result$se.diff
##***part105e;
sink()




# do the two sample Wilcoxon test
sink('baci-fish-R-107.txt',split=TRUE)
##***part107b;
result <- wilcox.test(diff ~ Period, data=fish.month, conf.int=TRUE)
result
##***part107e;
sink()





###############################################################################
# Do a Mixed effect linear model on the individual values


sink('baci-fish-R-300-type3.txt',split=TRUE)
##***part300b;
# Because there is ONLY one measurement per year, the SamplingTime*Site and
# residual variance are total confounded and cannot be separated. This is 
# the residual term.
result.lmer <- lmer(Count ~ SiteClass+Period+SiteClass:Period + (1|SamplingTimeF),
	              data=fish)
anova(result.lmer, ddf="Kenward-Roger")
##***part300e;
sink()

summary(result.lmer)

sink('baci-fish-R-300-vc.txt',split=TRUE)
##***part300vcb;
# Extract the variance components
vc <- VarCorr(result.lmer)
vc
##***part300vce;
sink()


# LSmeans after a lm() fit
sink('baci-fish-R-s300LSM-SiteClass.txt',split=TRUE)
##***parts300LSM-SiteClassb;
result.lmer.lsmo.S <- lsmeans::lsmeans(result.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass \n\n")
summary(result.lmer.lsmo.S)
##***parts300LSM-SiteClasse;
sink()

sink('baci-fish-R-300LSM-Period.txt',split=TRUE)
##***part300LSM-Periodb;
result.lmer.lsmo.P <- lsmeans::lsmeans(result.lmer, ~Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.lmer.lsmo.P)
##***part300LSM-Periode;
sink()

sink('baci-fish-R-300LSM-int.txt',split=TRUE)
##***part300LSM-intb;
result.lmer.lsmo.SP <- lsmeans::lsmeans(result.lmer, ~SiteClass:Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.lmer.lsmo.SP)
##***part300LSM-inte;
sink()

# Estimate the BACI contrast
# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.lmer)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]


sink("baci-fish-R-300baci.txt",split=TRUE)
##***part300bacib; 
# Estimate the BACI contrast along with a se
contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1)))
confint(contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1))))
##***part300bacie;
sink()




# Check the residuals etc

##***part300diagnosticb;
diagplot <- sf.autoplot.lmer(result.lmer)
diagplot
##***part300diagnostice;
ggsave(plot=diagplot, file='baci-fish-R-300-diagnostic.png', h=4, w=6, units="in", dpi=300)






