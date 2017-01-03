# BACI-Chironomid
# 2014-11-28 CJS sf.autoplot.lmer
# 2014-11-26 CJS sink, ggplot, ##***, lmer modifications

# Taken from Krebs, Ecological Methodology, 2nd Edition. Box 10.3.

# Estimates of chironomid abundance in sediments were taken at one station 
# above and below a pulp mill outflow pipe for 3 years before plant #operation 
# and for 6 years after plant operation.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lsmeans)
library(lmerTest)
library(plyr)

source("../../schwarz.functions.r")

# Read in the actual data
sink("baci-chironomid-R-001.txt", split=TRUE)
##***part001b;
cat(" BACI design measuring chironomid counts with multiple (paired) yearly measurements before/after \n\n")
chironomid <- read.csv("baci-chironomid.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
cat("Listing of part of the raw data \n")
head(chironomid)
##***part001e;
sink()

# The data is NOT in the usual format where there is only one column
# for the response and a separate column indicating if it is a control or 
# impact site. We need to restructure the data
sink('baci-chironomid-R-301.txt', split=TRUE)
##***part301b;
# We  reshape the data from wide to long format
chironomid.long <- reshape(chironomid, varying=c("Control.Site","Treatment.Site"),
     v.names="Count", direction="long",
     timevar=c("SiteClass"),
     times=c("Control","Impact"),
     drop=c("diff"), idvar=c("Year"))
chironomid.long$SiteClass     <-  factor(chironomid.long$SiteClass)
chironomid.long$Site          <-  factor(chironomid.long$Site)
chironomid.long$YearF         <-  factor(chironomid.long$Year)
chironomid.long$Period        <-  factor(chironomid.long$Period)
head(chironomid.long)
##***part301e;
sink()
str(chironomid.long)


# Get plot of series over time
##***part010b;
prelimplot <- ggplot(data=chironomid.long,
                     aes(x=Year, y=Count, group=Site, color=SiteClass, shape=Site))+
  ggtitle("Fish counts over time")+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=-0.5+min(chironomid.long$Year[as.character(chironomid.long$Period) == "After"]))
prelimplot
##***part010e;
ggsave(plot=prelimplot, file="baci-chironomid-R-010.png", h=4, w=6, units="in", dpi=300)



# There are several ways in which this BACI design can be analyzed.

###########################################################################
# Do a t-test on the differces of the averages for each site
# Because only one measurement was taken at each site in each year, we
# don't have to first average. We can use the wide format data.


sink('baci-chironomid-R-101.txt', split=TRUE)
##***part101b;
chironomid$diff <- chironomid$Treatment.Site - chironomid$Control.Site
head(chironomid)
##***part101e;
sink()


# Plot the difference over time
##***part102b;
plotdiff <- ggplot(data=chironomid, aes(x=Year, y=diff))+
  ggtitle("Plot of differences over time")+
  ylab("Difference (Impact-Control)")+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=-0.5+min(chironomid$Year[as.character(chironomid$Period) == "After"]))
plotdiff
##***part102e;
ggsave(plot=plotdiff, file="baci-chironomid-R-102.png", h=4, w=6, units="in", dpi=300)



# do the two sample t-test not assuming equal variances
sink('baci-chironomid-R-104.txt', split=TRUE)
##***part104b;
result <- try(t.test(diff ~ Period, data=chironomid),silent=TRUE)
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
sink('baci-chironomid-R-105.txt', split=TRUE)
##***part105b;
result <- t.test(diff ~ Period, data=chironomid, var.equal=TRUE)
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
sink('baci-chironomid-R-107.txt', split=TRUE)
##***part107b;
result <- wilcox.test(diff ~ Period, data=chironomid, conf.int=TRUE)
result
##***part107e;
sink()





##################################################################
# Do a Mixed effect linear model on the individual values




sink('baci-chironomid-R-300-type3.txt', split=TRUE)
##***part300b;
# Because there is ONLY one measurement per year, the SamplingTime*Site and
# residual variance are total confounded and cannot be separated. This is 
# the residual term.
result.lmer <- lmer(Count ~ SiteClass+Period+SiteClass:Period + (1|YearF),
                data=chironomid.long)
anova(result.lmer, ddf="Kenward-Roger")
##***part300e;
sink()

summary(result.lmer)


sink('baci-chironomid-R-300-vc.txt', split=TRUE)
##***part300vcb;
# Extract the variance components
vc <- VarCorr(result.lmer)
vc
##***part300vce;
sink()


# LSmeans after a lm() fit
sink('baci-chironomid-R-s300LSM-SiteClass.txt', split=TRUE)
##***parts300LSM-SiteClassb;
result.lmer.lsmo.S <- lsmeans::lsmeans(result.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass \n\n")
summary(result.lmer.lsmo.S)
##***parts300LSM-SiteClasse;
sink()

sink('baci-chironomid-R-300LSM-Period.txt', split=TRUE)
##***part300LSM-Periodb;
result.lmer.lsmo.P <- lsmeans::lsmeans(result.lmer, ~Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.lmer.lsmo.P)
##***part300LSM-Periode;
sink()

sink('baci-chironomid-R-300LSM-int.txt', split=TRUE)
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


sink("baci-chironomid-R-300baci.txt", split=TRUE)
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
ggsave(plot=diagplot, file='baci-chironomid-R-300-diagnostic.png',
       h=4, w=6, units="in", dpi=300)







