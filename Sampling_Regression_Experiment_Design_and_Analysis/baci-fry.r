# BACI-fry
# 2014-11-28 CJS sf.autoplot.lmer for diagnostic plots from lmer
# 2014-11-25 CJS ggplot; split; lmerTest; lsmeans changes

# This example is based (loosely) on a consulting project from an 
# Independent Power Producer who was interested in monitoring the 
# effects of an in-stream hydroelectric project. 

# The response variable for the project was the minnow density at 
# different locations in the stream.

# The  monitoring design has the river divided into six segments of 
# which three are upstream of the diversion
# and three are downstream of the diversion. In each segment, several sites 
# have been located where minnow fry 
# congregate. In each of the sites, minnow traps are set for various lengths 
# of Period. 

# At the end of the soaking period, the traps are removed and the number of 
# minnows are counted and classified by species.
# The counts are standardized to a common period of Period to adjust for the 
# different soak-time.  
# [This could be done directly in the analysis by using the soak-time as a 
# covariate, but the soak-time data was not available.]

# An initial pre-project monitoring study was run in 2000 to 2002. The 
# project became operational in late 2002, and
# post-project monitoring continued in 2003 and 2004.

# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

# A nice slide presentation on lmer() is found at 
#  http://www.stat.wisc.edu/~bates/PotsdamGLMM/LMMD.pdf

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lmerTest)
library(lsmeans)
library(plyr)

#source("../../schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')


# Read in the data
##***part001b;
sink('baci-fry-R-001.txt', split=TRUE)
fry <- read.csv('baci-fry.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
fry$logfry       <- log(fry$Fry)
fry$Period       <- factor(fry$Period)
fry$Site         <- factor(fry$Site)
fry$SiteClass    <- factor(fry$SiteClass)
fry$Period       <- factor(fry$Period)
fry$YearF        <- factor(fry$Year)
fry[1:12,]
##***part001e;
sink()

str(fry)


# look at the std dev to mean plot for the original and log() data 
# Compute the averages etc
sink('baci-fry-R-020.txt', split=TRUE)
##***part020b;
mean.fry <- ddply(fry, c("Year","YearF","Site","SiteClass","Period"), function(x){
  n <- nrow(x)
  nmiss <- sum(is.na(x$Fry))
  mean.fry <- mean(x$Fry, na.rm=TRUE)
  sd.fry   <- sd(  x$Fry, na.rm=TRUE)
  mean.logfry <- mean(x$logfry, na.rm=TRUE)
  sd.logfry   <- sd  (x$logfry, na.rm=TRUE)
  res<- data.frame(n, nmiss, mean.fry, sd.fry, mean.logfry, sd.logfry)
  })
mean.fry[1:5,]
##***part020e;
sink()


# Plot the std to mean ratios
##***part021b;
sdmeanplot <- ggplot(data=mean.fry, aes(x=mean.fry, y=sd.fry))+
  ggtitle("SD vs Mean - original data")+
  geom_point()
sdmeanplot
##***part021e;
ggsave(plot=sdmeanplot, file='baci-fry-R-300-diagnostic.png', h=4, w=6, units="in", dpi=300)


# Plot the std to mean ratios

##***part021b;
sdmeanplotlog <- ggplot(data=mean.fry, aes(x=mean.logfry, y=sd.logfry))+
  ggtitle("SD vs Mean - logged data")+
  geom_point()
sdmeanplotlog
##***part021e;
ggsave(plot=sdmeanplot, file='baci-fry-R-021b.png', h=4, w=6, units="in", dpi=300)


# Plot the mean log-fry over time by site
##***part025b;
meanplot <- ggplot(data=mean.fry, aes(x=Year, y=mean.logfry,
                  group=Site, color=SiteClass, shape=Site))+
  ggtitle("Changes in mean over time")+
  geom_point(position=position_dodge(w=0.2))+
  geom_line(position=position_dodge(w=0.2))+
  geom_vline(xintercept=-0.5+min(mean.fry$Year[as.character(mean.fry$Period) == "After"]))
meanplot
##***part025e;
ggsave(plot=meanplot, file="baci-fry-R-025.png", h=4, w=6, units="in", dpi=300)


#####################################################################################
# The analysis of the mean(log(Fry))

sink('baci-fry-R-200-type3.txt', split=TRUE)
##***part200b;
# Notice that we use the YearF rather than Year in the random statements
# Because we are analyzing the mean, the Site:Year term is the residual error
result.mean.lmer <- lmer(mean.logfry ~ Period + SiteClass + Period:SiteClass+
                      (1|YearF)+(1|Site),
                  data=mean.fry) 
anova(result.mean.lmer, ddf="Kenward-Roger")
##***part200e;
sink()


summary(result.mean.lmer)


sink('baci-fry-R-200-vc.txt', split=TRUE)
##***part200vcb;
# Extract the variance components
vc <- VarCorr(result.mean.lmer)
vc
##***part200vce;
sink()

# To extract the variance components for use in R, create a data frame
vc.df <- as.data.frame(vc)
YearSigma    <- vc.df[grep("YearF"   ,vc.df$grp),"sdcor"]
SiteSigma    <- vc.df[grep("Site"    ,vc.df$grp),"sdcor"]
ResidualSigma<- vc.df[grep("Residual",vc.df$grp),"sdcor"] 





# LSmeans after a lm() fit
sink('baci-fry-R-200LSM-SiteClass.txt', split=TRUE)
##***parts200LSM-SiteClassb;
result.mean.lmer.lsmo.S <- lsmeans::lsmeans(result.mean.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass \n\n")
summary(result.mean.lmer.lsmo.S)
##***parts200LSM-SiteClasse;
sink()

sink('baci-fry-R-200LSM-Period.txt', split=TRUE)
##***part200LSM-Periodb;
result.mean.lmer.lsmo.P <- lsmeans::lsmeans(result.mean.lmer, ~Period)
cat("\n\n Estimated marginal means for Period \n\n")
summary(result.mean.lmer.lsmo.P)
##***part200LSM-Periode;
sink()

sink('baci-fry-R-200LSM-int.txt', split=TRUE)
##***part200LSM-intb;
result.mean.lmer.lsmo.SP <- lsmeans::lsmeans(result.mean.lmer, ~SiteClass:Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.mean.lmer.lsmo.SP)
##***part200LSM-inte;
sink()

# Estimate the BACI contrast
# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.mean.lmer)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]

sink("baci-fry-R-200baci.txt", split=TRUE)
##***part200bacib; 
# Estimate the BACI contrast along with a se
contrast(result.mean.lmer.lsmo.SP, list(baci=c(1,-1,-1,1)))
confint(contrast(result.mean.lmer.lsmo.SP, list(baci=c(1,-1,-1,1))))
##***part200bacie;
sink()




# Check the residuals etc
##***part200diagnosticb;
diagplot <- sf.autoplot.lmer(result.mean.lmer)
diagplot
##***part200diagnostice;
ggsave(plot=diagplot, file='baci-fry-R-200-diagnostic.png', h=4, w=6, units="in", dpi=300)





#################################################################################
# The analysis of the individual values
# Results will differ slightly from answers on mean because design is unbalanced

sink('baci-fry-R-300-type3.txt', split=TRUE)
##***part300b;
# Notice that we use the YearF variable in the random effects
result.lmer <- lmer(logfry ~ Period + SiteClass + Period:SiteClass +
                      (1|YearF)+ (1|Site)+(1|YearF:Site), data=fry)
anova(result.lmer, ddf="Kenward-Roger")
##***part300e;
sink()


summary(result.lmer)


sink('baci-fry-R-300-vc.txt', split=TRUE)
##***part300vcb;
# Extract the variance components
vc <- VarCorr(result.lmer)
vc
##***part300vce;
sink()


# LSmeans after a lm() fit
sink('baci-fry-R-300LSM-SiteClass.txt', split=TRUE)
##***parts300LSM-SiteClassb;
result.lmer.lsmo.S <- lsmeans::lsmeans(result.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass \n\n")
summary(result.lmer.lsmo.S)
##***parts300LSM-SiteClasse;
sink()

sink('baci-fry-R-300LSM-Period.txt', split=TRUE)
##***part300LSM-Periodb;
result.lmer.lsmo.P <- lsmeans::lsmeans(result.lmer, ~Period)
cat("\n\n Estimated marginal means for Period \n\n")
summary(result.lmer.lsmo.P)
##***part300LSM-Periode;
sink()

sink('baci-fry-R-300LSM-int.txt', split=TRUE)
##***part300LSM-intb;
result.lmer.lsmo.SP <- lsmeans::lsmeans(result.lmer, ~SiteClass:Period)
cat("\n\n Estimated marginal means for SiteClass:Period \n\n")
summary(result.lmer.lsmo.SP)
##***part300LSM-inte;
sink()

# Estimate the BACI contrast
# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.lmer)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]


sink("baci-fry-R-300baci.txt", split=TRUE)
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
ggsave(plot=diagplot, file='baci-fry-R-300-diagnostic.png', h=4, w=6, units="in", dpi=300)


