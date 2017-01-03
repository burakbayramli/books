# Stream-biomass
# An example of pseudo-replication on regression analysis

# Measurements of biomass on a stream were taken at three times each Year.

# 2014-05-03 CJS First edition
# 2014-11-27 CJS ggplot; ##*** problem, split=TRUE; 

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmerTest)
library(lmtest)
library(plyr)
library(zyp) # sen non-parametric slope

source("../../schwarz.functions.r")


# Read in the data
sink('stream-biomass-R-010.txt', split=TRUE)
##***part010b;
biomass <- read.csv("stream-biomass.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
biomass$YearF <- factor(biomass$Year) # make a copy of the Year variable as a factor
head(biomass)
str(biomass)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=biomass, aes(x=Year, y=Biomass))+
  ggtitle("Stream biomass over time")+
  xlab("Year")+ylab("Biomass")+scale_x_discrete()+
  geom_point(position=position_jitter(h=0.2,w=0.1))+
  geom_smooth(method="lm", se=FALSE)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='stream-biomass-R-prelimplot.png', h=4, w=6, units="in", dpi=300)


# Fit the (incorrect) regression line and get the results
# Because of pseudo-replication, the standard errors are too small
# and the p-value is too small potentially leading to a false positive results.
sink('stream-biomass-R-regfitpseudo.txt', split=TRUE)
##***partregfitpseudob;
biomass.pseudo.fit <- lm( Biomass ~ Year, data=biomass)
summary(biomass.pseudo.fit)
##***partregfitpseudoe;
sink()

# The model diagnostics don't look bad, but not good either
plotdiag.pseudo <- sf.autoplot.lm(biomass.pseudo.fit)
plotdiag.pseudo

###############################################################################
###############################################################################
# Analyze the averages

sink('stream-biomass-R-avgpoints.txt', split=TRUE)
##***partavgpointsb;
# Because of the pseudo-replication, an approximate way to analyze
# the data is to first average over the pseudo-replicates and then
# fit a regression line to the averages.
# Using the averages in this way will be exact if the number of replicates
# at each time point is the same. 
biomass.avg <- ddply(biomass, "Year", function(x){
  # Compute the average biomass for each Years
  Biomass.avg <- mean(x$Biomass, na.rm=TRUE)
  return(data.frame(Biomass.avg))
})
biomass.avg
##***partavgpointse;
sink()

sink('stream-biomass-R-regfitavg.txt', split=TRUE)
##***partregfitavgb;
biomass.avg.fit <- lm( Biomass.avg ~ Year, data=biomass.avg)
summary(biomass.avg.fit)
##***partregfitavge;
sink()


sink("stream-biomass-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(biomass.avg.fit)
coef(biomass.avg.fit)
sqrt(diag(vcov(biomass.avg.fit))) # gives the SE
confint(biomass.avg.fit)
names(summary(biomass.avg.fit))
summary(biomass.avg.fit)$r.squared
summary(biomass.avg.fit)$sigma
##***partfitpiecese;
sink()

##***partdiagplotb;
# look at diagnostic plot
plotdiag.avg <- sf.autoplot.lm(biomass.avg.fit)
plotdiag.avg
##***partdiagplote;
ggsave(plot=plotdiag.avg, file='stream-biomass-R-diagplot-avg.png', h=4, w=6, units="in", dpi=300)


sink("stream-biomass-R-dwtest-avg.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

durbinWatsonTest(biomass.avg.fit) # from the car package
dwtest(biomass.avg.fit) # from the lmtest package
##***partdwteste;
sink()

##***partplotfitavgb;
# plot the fitted line to the graphs
plotfit.avg <- plotprelim + 
     geom_abline(intercept=coef(biomass.avg.fit)[1], slope=coef(biomass.avg.fit)[2],
                 color="red",size=2,linetype="dashed")
plotfit.avg
##***partplotfitavge;

ggsave(plot=plotfit.avg, file='stream-biomass-R-plotfit-avg.png', h=4, w=6, units="in", dpi=300)

sink('stream-biomass-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(Year=seq(min(biomass$Year,na.rm=TRUE),max(biomass$Year,na.rm=TRUE),1))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('stream-biomass-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg.avg <- predict(biomass.avg.fit, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.avg.df <- cbind(newYears, predict.avg.avg$fit, se=predict.avg.avg$se.fit)
tail(predict.avg.avg.df)

# Add the confidence intervals to the plot
plotfit.avg.avg.ci <- plotfit.avg +
    geom_ribbon(data=predict.avg.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avg.avg.ci
##***partpredavge;
sink()

ggsave(plot=plotfit.avg.avg.ci, file='stream-biomass-R-plotpredavg.png', h=4, w=6, units="in", dpi=300)



sink('stream-biomass-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.avg.indiv <- predict(biomass.avg.fit, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.avg.indiv.df <- cbind(newYears, predict.avg.indiv)
tail(predict.avg.indiv.df)

# Add the prediction intervals to the plot
plotfit.avg.indivci <- plotfit.avg.avg.ci +
    geom_ribbon(data=predict.avg.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.avg.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.avg.indivci, file='stream-biomass-R-plotpredindiv.png', h=4, w=6, units="in", dpi=300)


# No easy way to do inverse predictions


# Kendall test for trend
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

tau.test <- MannKendall(biomass.avg$Biomass.avg)
summary(tau.test)

# Now to estimate the slope using the Sen estimator in the zyp package
sen.slope <- zyp.sen(Biomass.avg~Year, data=biomass.avg)
sen.slope$coef
confint.zyp(sen.slope)




###############################################################################
###############################################################################
# Analyze the individual data points

##***partcreatecopyb;
# You can use the individual data point, by fitting a mixed linear model
# You will need to create a new variable that is a copy of the time variable
# and then declared as a factor. This was the YearF variable defined when the data
# was read in.
str(biomass)
##***partcreatecopye;

sink('stream-biomass-R-regfitindiv.txt', split=TRUE)
##***partregfitindivb;
biomass.indiv.fit <- lmer(Biomass ~ Year + (1|YearF), data=biomass)
summary(biomass.indiv.fit)
anova(biomass.indiv.fit, ddfm='Kenward-Roger')
##***partregfitindive;
sink()

sink("stream-biomass-R-regfitindiv-vc.txt", split=TRUE)
##***partregfitindivvcb;
# extract the random effect variance components
VarCorr(biomass.indiv.fit)
##***partregfitindivvce;
sink()

# making predictions is MUCH harder for lmer() models and really is not needed
# because the model using the averages is just fine.


##########################################################################
##########################################################################
# get the power functions
source("../../Power/RegPower/SLR-power/slr-power-stroup.r")

# Power analysis using the data from the fit to the average values.
# Now the residual std dev is a combination of process and sampling error
# We can investigate changes in the number of years of sampling, but each year
# is assumed to have exactly 3 measurement (the average number observed in the fit on
# the averages)

Process.SD <- summary(biomass.avg.fit)$sigma
Sampling.SD <- 0

# We think that an trend line of -1/year (not a percentage!) is important
# What is the power after 10 years of sampling each year?
Xvalues <- 1:10
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


# What is the power after 15 years of sampling each year?
Xvalues <- 1:15
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


# What is the power after 15 years of sampling each year?
Xvalues <- 1:20
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)

# How about if we skip sampling every second year?
Xvalues <- seq(1,20,2)
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


# Now if we want to modify both the number of years of sampling and 
# the amount of sampling within a year, we need to separate the process and sampling errors

# Power anlaysis using the information from the individual fit.
# We need to extract the process and sampling standard deviations

vc <- as.data.frame(VarCorr(biomass.indiv.fit)) # extract the variance components
vc

Process.SD  <- vc[ grep("YearF", vc$grp), "sdcor"]
Sampling.SD <- vc[ grep("Resid", vc$grp), "sdcor"]


# To specify multiple measurements each year, use the rep() function
Xvalues <- rep(1:10, each=3)  # 3 measurements in each year
cat("Power for X values: ", Xvalues, "\n")

# Want to detect an ABSOLUTE slope of -1 using 10 years of sampling and 3 samples/year
# result is very close to result from avg, but differs slightly because lmer
# uses a slight different algorithm to estimate variance components
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)

# What happends if we increase the number of samples/year to 6?
# It turns out that because process error is so large relative to sampling error
# that multiple measurements within a year are not really useful.
Xvalues <- rep(1:10, each=6)  # 6 measurements in each year
cat("Power for X values: ", Xvalues, "\n")
slr.power.stroup(Trend=-1, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)





