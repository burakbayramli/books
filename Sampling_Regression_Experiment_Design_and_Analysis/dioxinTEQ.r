# Degradation of dioxin in crabs.
# 2014-11-28 CJS fix ##*** problem; add power analysis; remove Base R graphics
# 2014-05-02 CJS ggplot, etc

# How fast does dioxin degrade over time? In each year, samples of crabs
# were captured at a site. The crab livers were excised, composited 
# together, and various species of dioxins were measured. These were 
# translated into the World Health Organization (WHO) standardized total #equivalent dose.


# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(randtests)
library(zyp)

source("../../schwarz.functions.r")



# Read in the data
sink('dioxinTEQ-R-010.txt', split=TRUE)
##***part010b;
crabs <- read.csv("dioxinTEQ.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
head(crabs)
str(crabs)
##***part010e;
sink()

# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=crabs, aes(x=year, y=WHO.TEQ))+
  ggtitle("Dioxin levels over time")+
  xlab("Year")+ylab("Dioxin levels (WHO.TEQ)")+
  geom_point()
plotprelim
##***partprelimplote;
ggsave(plot=plotprelim, file='dioxinTEQ-R-prelimplot.png')



# Find the logTEQ and then get a revised plot

sink('dioxinTEQ-R-002a.txt', split=TRUE)
##***part002ab;
crabs$logTEQ <- log(crabs$WHO.TEQ)
head(crabs)
##***part002ae;
sink()

# Repeat the plots on the log-scale
##***partprelimplotb;
plotprelimlog <- ggplot(data=crabs, aes(x=year, y=logTEQ))+
  ggtitle("log(Dioxin) levels over time")+
  xlab("Year")+ylab("log(Dioxin) levels (WHO.TEQ)")+
  geom_point()
plotprelimlog
##***partprelimplote;
ggsave(plot=plotprelimlog, file='dioxinTEQ-R-prelimplotlog.png')


# Fit the regression line and get the results
sink('dioxinTEQ-R-regfit.txt', split=TRUE)
##***partregfitb;
crabs.fit <- lm( logTEQ ~ year, data=crabs)
summary(crabs.fit)
##***partregfite;
sink()

sink("dioxinTEQ-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(crabs.fit)
coef(crabs.fit)
sqrt(diag(vcov(crabs.fit))) # gives the SE
confint(crabs.fit)
names(summary(crabs.fit))
summary(crabs.fit)$r.squared
summary(crabs.fit)$sigma
##***partfitpiecese;
sink()


##***partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(crabs.fit)
plotdiag
##***partdiagplote;
ggsave(plot=plotdiag, file='dioxinTEQ-R-diagplot.png')



sink("dioxinTEQ-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

durbinWatsonTest(crabs.fit) # from the car package
dwtest(crabs.fit) # from the lmtest package
##***partdwteste;
sink()


##***partplotfitb;
# plot the fitted line to the graphs
plotfit <- plotprelimlog + 
     geom_abline(intercept=coef(crabs.fit)[1], slope=coef(crabs.fit)[2])
plotfit
##***partplotfite;

ggsave(plot=plotfit, file='dioxinTEQ-R-plotfit.png')

# or we can use the builtin smoothing functions of ggplot which also gives 
# a confidence interval for the MEAN response
plotfit2 <- plotprelimlog +
     geom_smooth(method="lm")
plotfit2


sink('dioxinTEQ-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newyears <- data.frame(year=seq(min(crabs$year,na.rm=TRUE),2030,1))
newyears[1:5,]
str(newyears)
##***partpredsetupe;
sink()

sink('dioxinTEQ-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE dioxin level at each year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(crabs.fit, newdata=newyears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newyears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)
predict.avg.df[predict.avg.df$year==2010,]
exp(predict.avg.df[predict.avg.df$year==2010,])
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)+
    xlim(c(1990,2010))
plotfit.avgci
##***partpredavge;
sink()

ggsave(plot=plotfit.avgci, file='dioxinTEQ-R-plotpredavg.png')

# You can also plot the same data on the anti-log scale
plotfit.avgci.antilog <- plotprelim +
    geom_line( data=predict.avg.df, aes(x=year, y=exp(fit)))+
    geom_ribbon(data=predict.avg.df, aes(x=year,y=NULL, ymin=exp(lwr), ymax=exp(upr)),alpha=0.2)+
    xlim(c(1990,2010))
plotfit.avgci.antilog


# You can plot everything back on the anti-logscale as well - try it!


sink('dioxinTEQ-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL dioxin levels n each year
# This is a bit strange because the data points are the dioxin level in a composite
# sample and not individual crabs. So these prediction intervals
# refer to the range of composite values and not the
# levels in individual crabs.
# R does not product the se for individual predictions
predict.indiv <- predict(crabs.fit, newdata=newyears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newyears, predict.indiv)
head(predict.indiv.df)
predict.indiv.df    [predict.indiv.df$year==2010,]
exp(predict.indiv.df[predict.indiv.df$year==2010,])

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='dioxinTEQ-R-plotpredindiv.png')

# add to the antilog plot
plotfit.indivci.antilog <- plotfit.avgci.antilog +
    geom_ribbon(data=predict.indiv.df, aes(x=year,y=NULL, ymin=exp(lwr), ymax=exp(upr)),alpha=0.1)
plotfit.indivci.antilog


# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended

##***partinvpredb;
plotinvpred <- plotfit.indivci +
  geom_hline(yintercept=log(10))+
  xlim(c(1990, 2030))
plotinvpred
##***partinvprede;

ggsave(plot=plotinvpred, file='dioxinTEQ-R-plotinvpred.png')




###################################################################################
# Cox and Stuart Test for trend
# This is simply a sign.test applied to the paired data
# It is available in the randtests package
# You must make sure that the data is sorted by time ordering and that all missing
# values are removed

sink("dioxinTEQ-R-coxstuart.txt",split=TRUE)
##***partcoxstuartb;
crabs <- crabs[order(crabs$year),]
logTEQ.no.missing <- crabs$logTEQ[!is.na(crabs$logTEQ)]
cox.stuart.test(logTEQ.no.missing)
##***partcoxstuarte;
sink()

# Note that the Cox and Stuart test gives the same result using
# the untransformed data as well.
crabs <- crabs[order(crabs$year),]
WHO.TEQ.no.missing <- crabs$WHO.TEQ[!is.na(crabs$WHO.TEQ)]
cox.stuart.test(WHO.TEQ.no.missing)


##############################################################################
# Spearman's rho (non-parametric correlation)

sink('dioxinTEQ-R-spearman.txt',split=TRUE)
##***partspearmanb;
# Because there is no distiction between the response and predictor variables,
# the formula is specified with both variables to the right
# of the tilde
cor.test( ~logTEQ + year, data=crabs, method="spearman")
##***partspearmane;
sink()

# Note that the Spearman rho is the same if the original data are used
cor.test( ~WHO.TEQ + year, data=crabs, method="spearman")
##***partspearmane;

###################################################################################
# Kendall test for trend
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# The MannKendall test is found in the Kendall package
# or it can be computed using the cor.test() function as well

sink('dioxinTEQ-R-kendalltau.txt',split=TRUE)
##***partkendalltaub;
tau.test <- Kendall(crabs$logTEQ, crabs$year)
summary(tau.test)
tau.test2 <-cor.test( ~logTEQ + year, data=crabs, method="kendall")
tau.test2
##***partkendalltaue;
sink()

sink('dioxinTEQ-R-senslope.txt',split=TRUE)
##***partsenslopeb;
# Now to estimate the slope using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(logTEQ~year, data=crabs)
sen.slope$coef
confint.zyp(sen.slope)
##***partsenslopee;
sink()


# Note that the Kendall tau is the same if the original data are 
# used but the Sen estimate of the slope differs as they are on
# diffent units (one logged and one not logged).

tau.test <- Kendall(crabs$WHO.TEQ, crabs$year)
summary(tau.test)
tau.test2 <-cor.test( ~WHO.TEQ + year, data=crabs, method="kendall")
tau.test2

# Now to estimate the slope using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(WHO.TEQ~year, data=crabs)
sen.slope$coef
confint.zyp(sen.slope)


###################################################################################
# Power analyses
# Because we have a single measurement per year, process and sampling error
# are completely confounded together and cannot be separated.

# Get the power function
source("../../Power/RegPower/SLR-Power/slr-power-stroup.r")

# (a) on the absolute trend scale
# Here it makes no sense to consider this type of trend. Like radio-active decay, the degradation of
# dioxin occurs on a MULTIPLICATE scale so a constant number of units removed/year makes no sense.


# (b) a proportion change (percentage change) per year
# We sometimes are interested in a proportional (percentage) change per year. For example, how many
# year do we need to sample to detect a 5% increase/year. We take advantage of the special form of the
# natural log, where a value of .05 on the log-scale corresponds to a (very close) approximation to a 1%
# increase per year.

# Our original analysis is on the log-scale, so life is easy

Process.SD.log   <- summary(crabs.fit)$sigma
Sampling.SD.log  <- 0 

# We are interested in detecting a 5% decline/year
Trend.prop <- -.05

Xvalues <- 1:10 # 10 years of yearly sampling
slr.power.stroup( Trend=Trend.prop, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD.log, Sampling.SD=Sampling.SD.log, alpha=0.05)

Xvalues <- 1:20 # 20 years of yearly sampling
slr.power.stroup( Trend=Trend.prop, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD.log, Sampling.SD=Sampling.SD.log, alpha=0.05)

Xvalues <- seq(1,20,2) # 20 years of biennial sampling
slr.power.stroup( Trend=Trend.prop, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD.log, Sampling.SD=Sampling.SD.log, alpha=0.05)





