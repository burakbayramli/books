# BACI design with multiple controls; 2 factor; interaction;
# 2015-07-15 CJS update misc topics
# 2014-11-27 CJS added sf.autoplot.lmer
# 2014-11-26 CJS split; ggplot; ##--- problem; use lmerTest;

# A BACI design was used to assess the impact 
#   of cooling water discharge on the density of 
#   shore crabs. 

#   The beach near the outlet of the cooling water 
#   was sampled using several quadrats
#   before and after the plant started operation. 
#   Two control beaches at other locations
#   in the inlet were also sampled. 

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lmerTest)
library(lsmeans)
library(plyr)
library(reshape2)

source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")


# Read in the actual data
sink("baci-crabs-mod-R-001.txt", split=TRUE)
##***part001b;
cat(" BACI design measuring crab Density with multiple sites but one year before/after \n\n")

crabs <- read.csv("baci-crabs-mod.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
crabs$trt <- interaction(crabs$SiteClass, crabs$Site, crabs$Period)
crabs$Site      <- factor(crabs$Site)
crabs$SiteClass <- factor(crabs$SiteClass)
crabs$Period    <- factor(crabs$Period, levels=c("Before","After"), ordered=TRUE)
crabs$trt       <- factor(crabs$trt)
cat("Listing of part of the raw data \n")
crabs[1:10,]
##***part001e;
sink()

str(crabs)

# Preliminary plot

# Get side-by-side dot plots
##***part010b;
prelimplot <- ggplot(data=crabs, aes(x=trt, y=Density))+
  ggtitle("Preliminary plot to look for outliers etc")+
  geom_point(position=position_jitter(w=0.1))+
  geom_boxplot(alpha=0.1)
prelimplot
##***part010e;
ggsave(plot=prelimplot, file="baci-crabs-mod-R-010.png",
       h=4, w=6, units="in", dpi=300)



# Get some simple summary statistics
sink('baci-crabs-mod-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(crabs, c("Period","SiteClass","Site"), function(x){
   res <- sf.simple.summary(x, "Density", crd=TRUE)
   return(res)
})
cat("\n\n Summary report \n")
report
##***part020e;
sink()

# Draw a profile plot
##***part030b;
profileplot <- ggplot(data=report, aes(x=Period, y=mean, 
                    group=Site, color=SiteClass, shape=Site))+
  ggtitle("Profile plot of crab density")+
  ylab("Density with mean and 95% ci")+
  geom_point(position=position_dodge(w=0.1))+
  geom_line(position=position_dodge(w=0.1))+
  geom_errorbar(aes(ymax=ucl, ymin=lcl), width=0.2, position=position_dodge(w=0.1))+
  geom_point(data=crabs, aes(y=Density), position=position_dodge(w=0.2))
profileplot
##***part030e;
ggsave(plot=profileplot, file="baci-crabs-mod-R-030.png",
       h=4, w=6, units="in", dpi=300)


# There are several ways in which this BACI design can be analyzed.

#################################################################################
#################################################################################
# Do a t-test on the differeces of the averages for each site

sink('baci-crabs-mod-R-100.txt', split=TRUE)
##***part100b;
# The averages are available in the report dataframe
crabs.avg <- report[,c("Site","SiteClass","Period","mean")]
crabs.avg
##***part100e;
sink()

sink('baci-crabs-mod-R-101.txt', split=TRUE)
##***part101b;
# Reshape the file to get the Before and After measurements on the same line
# 
crabs.site <- dcast(crabs.avg, Site+SiteClass ~ Period, value.var="mean" )
crabs.site$diff <- crabs.site$After - crabs.site$Before
crabs.site
##***part101e;
sink()


# do the two sample t-test not assuming equal variances
# Unfortunately, you need at least 2 sites in EACH SiteClass, so this does not work here
sink('baci-crabs-mod-R-104.txt', split=TRUE)
##***part104b;
result <- try(t.test(diff ~ SiteClass, data=crabs.site),silent=TRUE)
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
# This only requires at least 2 sites in at least one of the SiteClasses
sink('baci-crabs-mod-R-105.txt', split=TRUE)
##***part105b;
result <- t.test(diff ~ SiteClass, data=crabs.site, var.equal=TRUE)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- result$statistic / abs(result$diff.in.means)
names(result$se.diff) <- 'SE.diff'
result
result$diff.in.means
result$se.diff
##***part105e;
sink()


#################################################################################
#################################################################################
# Do a Mixed effect linear model on the  averages for each site

crabs.avg <- report[,c("Site","SiteClass","Period","mean")]
crabs.avg

sink('baci-crabs-mod-R-200-type3.txt', split=TRUE)
##***part200b;
result.lmer <- lmerTest::lmer(mean ~ SiteClass+Period+SiteClass:Period +
                    (1|Site), data=crabs.avg)
anova(result.lmer, ddf="Kenward-Roger")
##***part200e;
sink()



sink('baci-crabs-mod-R-200-vc.txt', split=TRUE)
##***part200vcb;
# Extract the variance components
vc <- VarCorr(result.lmer)
vc
##***part200vce;
sink()



# Extract the BACI effect 
# You could get this from the summary table looking at the 
# interaction term, but it is more robust to get this from the
# lsmeans
summary(result.lmer)

# LSmeans after a lm() fit
# Note that there is a lsmeans() function in both the lsmeans and lmerTest package
# so we must specify which one we want
sink('baci-crabs-mod-R-s00LSM-SiteClass.txt', split=TRUE)
##***parts200LSM-SiteClassb;
result.lmer.lsmo.S <- lsmeans::lsmeans(result.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass \n\n")
summary(result.lmer.lsmo.S)
##***parts200LSM-SiteClasse;
sink()

sink('baci-crabs-mod-R-200LSM-Period.txt', split=TRUE)
##***part200LSM-Periodb;
result.lmer.lsmo.P <- lsmeans::lsmeans(result.lmer, ~Period)
cat("\n\n Estimated marginal means for Period \n\n")
summary(result.lmer.lsmo.P)
##***part200LSM-Periode;
sink()

sink('baci-crabs-mod-R-200LSM-int.txt', split=TRUE)
##***part200LSM-intb;
result.lmer.lsmo.SP <- lsmeans::lsmeans(result.lmer, ~SiteClass:Period)
cat("\n\n Estimated marginal means for SiteClass:Period \n\n")
summary(result.lmer.lsmo.SP)
##***part200LSM-inte;
sink()


# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.lmer)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]


sink("baci-crabs-mod-R-200baci.txt", split=TRUE)
##***part200bacib; 
# Estimate the BACI contrast along with a se
contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1)))
confint(contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1))))
##***part200bacie;
sink()




# Check the residuals etc
##***part200diagnosticb;
diagplot <- sf.autoplot.lmer(result.lmer)
diagplot
##***part200diagnostice;
ggsave(plot=diagplot, file='baci-crabs-mod-R-200-diagnostic.png',
       h=4, w=6, unit="in", dpi=300)






#################################################################################
#################################################################################
# Do a Mixed effect linear model on the individual values
# Results will differ slightly from above analyeses on the averages because
# the design is not balanced.


sink('baci-crabs-mod-R-300-type3.txt', split=TRUE)
##***part300b;
result.all.lmer <- lmer(Density ~ SiteClass+Period+SiteClass:Period +
                 (1|Site) + (1|Site:Period), data=crabs)
anova(result.all.lmer, ddf="Kenward-Roger")
##***part300e;
sink()

summary(result.all.lmer)


sink('baci-crabs-mod-R-300-vc.txt', split=TRUE)
##***part300vcb;
# Extract the variance components
vc <- VarCorr(result.all.lmer)
vc
##***part300vce;
sink()


# LSmeans after a lm() fit
sink('baci-crabs-mod-R-s300LSM-SiteClass.txt', split=TRUE)
##***parts300LSM-SiteClassb;
result.all.lmer.lsmo.S <- lsmeans::lsmeans(result.all.lmer, ~SiteClass)
cat("\n\n Estimated marginal means for SiteClass\n\n")
summary(result.all.lmer.lsmo.S)
##***parts300LSM-SiteClasse;
sink()

sink('baci-crabs-mod-R-300LSM-Period.txt', split=TRUE)
##***part300LSM-Periodb;
result.all.lmer.lsmo.P <- lsmeans::lsmeans(result.all.lmer, ~ Period)
cat("\n\n Estimated marginal means for Period \n\n")
summary(result.all.lmer.lsmo.P)
##***part300LSM-Periode;
sink()

sink('baci-crabs-mod-R-300LSM-int.txt', split=TRUE)
##***part300LSM-intb;
result.all.lmer.lsmo.SP <- lsmeans::lsmeans(result.all.lmer, ~SiteClass:Period)
cat("\n\n Estimated marginal means for SiteClass:Period \n\n")
summary(result.all.lmer.lsmo.SP)
##***part300LSM-inte;
sink()


# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.all.lmer)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]


sink("baci-crabs-mod-R-300baci.txt", split=TRUE)
##***part300bacib; 
# Estimate the BACI contrast along with a se
contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1)))
confint(contrast(result.lmer.lsmo.SP, list(baci=c(1,-1,-1,1))))
##***part300bacie;
sink()




# Check the residuals etc
##***part300diagnosticb;
diagplot <- sf.autoplot.lmer(result.all.lmer)
diagplot
##***part300diagnostice;
ggsave(plot=diagplot, file='baci-crabs-mod-R-300-diagnostic.png',
              h=4, w=6, units="in", dpi=300)



