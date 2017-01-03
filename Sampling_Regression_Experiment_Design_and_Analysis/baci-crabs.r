# BACI-crabs
# 2015-07-13 CJS update lm() to use contrast for Type III SS
# 2012-11-20 CJS update to use ggplot; lsmeans; split on sinks, ##*** problem

# A simple BACI design was used to assess the impact of cooling water 
# discharge on the Density of shore crabs. 
# The beach near the outlet of the cooling water was sampled 
# using several quadrats before and after the plant started operation, 
# as was a control beach on the other side of the body of water.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lsmeans)
library(plyr)

source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the actual data and define the factors of interest
sink("baci-crabs-R-001.txt")
##***part001b;
cat(" BACI design measuring crab Density \n\n")

crabs <- read.csv("baci-crabs.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
crabs$trt <- interaction(crabs$SiteClass, crabs$Period)
crabs$SiteClass <- factor(crabs$SiteClass)
crabs$Period    <- factor(crabs$Period, levels=c("Before","After"), ordered=TRUE) # sort correctly
crabs$trt       <- factor(crabs$trt)
cat("Listing of part of the raw data \n")
crabs[1:10,]
##***part001e;
sink()

str(crabs)
# Preliminary plot

##***part010b;
prelimplot <- ggplot(data=crabs, aes(x=trt, y=Density))+
  ggtitle("Preliminary plot to look for outliers etc")+
  geom_point(position=position_jitter(w=0.1))+
  geom_boxplot(alpha=0.1)
prelimplot
##***part010e;

ggsave(plot=prelimplot, file="baci-crabs-R-010.png", h=4, w=6, units="in", dpi=300)

 

# Get some simple summary statistics
sink('baci-crabs-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(crabs, c("Period","SiteClass"), function(x){
   res <- sf.simple.summary(x, "Density", crd=TRUE)
   return(res)
})
cat("\n\n Summary report \n")
report
##***part020e;
sink()

# Draw a profile plot includeing the raw data
##***part030b;
profileplot <- ggplot(data=report, aes(x=Period, y=mean, group=SiteClass, color=SiteClass))+
  ggtitle("Profile plot of crab density")+
  ylab("Density with mean and 95% ci")+
  geom_point(position=position_dodge(w=0.1))+
  geom_line(position=position_dodge(w=0.1))+
  geom_errorbar(aes(ymax=ucl, ymin=lcl), width=0.2, position=position_dodge(w=0.1))+
  geom_point(data=crabs, aes(y=Density), position=position_dodge(w=0.2))
profileplot
##***part030e;

ggsave(plot=profileplot, file="baci-crabs-R-030.png", h=4, w=6, units="in", dpi=300) 

# Fit the linear model, get the anova table, and the usual stuff
# CAUTION!!! Because the design is unbalanced, the default model
# fit by aov gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html
#
sink('baci-crabs-R-100.txt', split=TRUE)
##***part100b;
cat("The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
result.lm <- lm( Density ~ SiteClass + Period + SiteClass:Period, data=crabs)
cat("\n\n Analysis of variance -- this is NOT CORRECT because design is unbalanced \n")
anova(result.lm)

cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")
library(car)
result.lm2 <- lm( Density ~ SiteClass + Period + SiteClass:Period, data=crabs,
                  contrasts=list(SiteClass="contr.sum", Period='contr.sum'))
Anova(result.lm2,type=3)
##***part100e;
sink()




# Check the residuals etc

##***part100diagnosticb;
diagplot <- sf.autoplot.lm(result.lm)
diagplot
##***part100diagnostice;
ggsave(plot=diagplot, file='baci-crabs-R-100-diagnostic.png', h=4, w=6, units="in", dpi=300)


# Estimate the marginal means and the various effects
sink('baci-crabs-R-100LSM-SiteClass.txt', split=TRUE)
##***part100LSM-SiteClassb;
result.lsmo.S <- lsmeans::lsmeans(result.lm, ~SiteClass)
cat("\n\n Estimated marginal means \n\n")
summary(result.lsmo.S)
##***part100LSM-SiteClasse;
sink()

sink('baci-crabs-R-100LSM-Period.txt', split=TRUE)
##***part100LSM-Periodb;
result.lsmo.P <- lsmeans::lsmeans(result.lm, ~Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.lsmo.P)
##***part100LSM-Periode;
sink()

sink('baci-crabs-R-100LSM-int.txt', split=TRUE)
##***part100LSM-intb;
result.lsmo.SP <- lsmeans::lsmeans(result.lm, ~SiteClass:Period)
cat("\n\n Estimated marginal means \n\n")
summary(result.lsmo.SP)
##***part100LSM-inte;
sink()

# Estimate the BACI contrast along with a se
# You could look at the entry in the summary table from the model fit, but
# this is dangerous as these entries depend on the contrast matrix.
# It is far safer to the contrast function applied to an lsmeans object
temp <- summary(result.lm)$coefficients # get all the coefficients
temp[grepl("SiteClass",rownames(temp)) & grepl("Period", rownames(temp)),]

sink("baci-crabs-R-100baci.txt", split=TRUE)
##***part100bacib;
contrast(result.lsmo.SP, list(baci=c(1,-1,-1,1)))
confint(contrast(result.lsmo.SP, list(baci=c(1,-1,-1,1))))
##***part100bacie;
sink()
