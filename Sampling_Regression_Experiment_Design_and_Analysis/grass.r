# The Grass is Greener (and grows longer)
# 2014-11-28 CJS Remove Base R graphics, Added power analysis; ##*** problem 
# 2014-05-03 CJS First edition

# Taken from
#    Sparks, T.H., Croxton, J.P.J., Collinson, N., and Grisenthwaite, D.A. (2005)
#    The Grass is Greener (for longer).
#`   Weather 60,  121-123.

#    D.G. Grisenthwaite, a pensioner who has spent 20 years keeping detailed records 
#    of how often he cuts his grass has been included in a climate change study. 
#    David Grisenthwhaite, 77, and a self-confessed "creature of habit", 
#    has kept a note of cutting grass in his Kirkcaldy garden since 1984. 
#    The grandfather's data was so valuable it was used by 
#    the Royal Meteorological Society in a paper on global warming.

#    The retired paper-maker, who moved to Scotland from Cockermouth
#    in West Cumbria in 1960, said he began making a note of the time and 
#    date of every occasion he cut the grass simply "for the fun of it".

#    I have extracted the data on the cutting duration from the above paper. 

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
sink('grass-R-010.txt', split=TRUE)
##***part010b;
grass <- read.csv("grass.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
head(grass)
str(grass)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=grass, aes(x=year, y=days))+
  ggtitle("Cutting duration over time")+
  xlab("Year")+ylab("Cutting duration (days)")+
  geom_point()
plotprelim
##***partprelimplote;
ggsave(plot=plotprelim, file='grass-R-prelimplot.png')


# Fit the regression line and get the results
sink('grass-R-regfit.txt', split=TRUE)
##***partregfitb;
grass.fit <- lm( days ~ year, data=grass)
summary(grass.fit)
##***partregfite;
sink()

sink("grass-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(grass.fit)
coef(grass.fit)
sqrt(diag(vcov(grass.fit))) # gives the SE
confint(grass.fit)
names(summary(grass.fit))
summary(grass.fit)$r.squared
summary(grass.fit)$sigma
##***partfitpiecese;
sink()

##***partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(grass.fit)
plotdiag
##***partdiagplote;
ggsave(plot=plotdiag, file='grass-R-diagplot.png')



sink("grass-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

durbinWatsonTest(grass.fit) # from the car package
dwtest(grass.fit) # from the lmtest package
##***partdwteste;
sink()

##***partplotfitb;
# plot the fitted line to the graphs
plotfit <- plotprelim + 
     geom_abline(intercept=coef(grass.fit)[1], slope=coef(grass.fit)[2])
plotfit
##***partplotfite;

ggsave(plot=plotfit, file='grass-R-plotfit.png')

# or we can use the builtin smoothing functions of ggplot which also gives 
# a confidence interval for the MEAN response
plotfit2 <- plotprelim +
     geom_smooth(method="lm")
plotfit2



sink('grass-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newyears <- data.frame(year=seq(min(grass$year,na.rm=TRUE),max(grass$year,na.rm=TRUE),1))
newyears[1:5,]
str(newyears)
##***partpredsetupe;
sink()

sink('grass-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(grass.fit, newdata=newyears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newyears, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()
ggsave(plot=plotfit.avgci, file='grass-R-plotpredavg.png')


sink('grass-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each year
# R does not product the se for individual predictions
predict.indiv <- predict(grass.fit, newdata=newyears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newyears, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='grass-R-plotpredindiv.png')


# No easy way to do inverse predictions


###################################################################################
# Cox and Stuart Test for trend
# This is simply a sign.test applied to the paired data
# It is available in the randtests package
# You must make sure that the data is sorted by time ordering and that all missing
# values are removed

sink("grass-R-coxstuart.txt",split=TRUE)
##***partcoxstuartb;
grass <- grass[order(grass$year),]
days.no.missing <- grass$days[!is.na(grass$days)]
cox.stuart.test(days.no.missing)
##***partcoxstuarte;
sink()


##############################################################################
# Spearman's rho (non-parametric correlation)

sink('grass-R-spearman.txt',split=TRUE)
##***partspearmanb;
# Because there is no distiction between the response and predictor variables,
# the formula is specified with both variables to the right
# of the tilde
cor.test( ~days + year, data=grass, method="spearman")
##***partspearmane;
sink()

###################################################################################
# Kendall test for trend
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# The Mann-Kendall test is found in the Kendall package
# or it can be computed using the cor.test() function as well

sink('grass-R-kendalltau.txt',split=TRUE)
##***partkendalltaub;
tau.test <- Kendall(grass$days, grass$year)
summary(tau.test)
tau.test2 <-cor.test( ~days + year, data=grass, method="kendall")
tau.test2
##***partkendalltaue;
sink()

sink('grass-R-senslope.txt',split=TRUE)
##***partsenslopeb;
# Now to estimate the slope using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(days~year, data=grass)
sen.slope$coef
confint.zyp(sen.slope)
##***partsenslopee;
sink()


###################################################################################
# Power analyses
# Because we have a single measurement per year, process and sampling error
# are completely confounded together and cannot be separated.

# Get the power function
source("../../Power/RegPower/SLR-Power/slr-power-stroup.r")

# (a) on the absolute trend scale
# Here we want to determine the number of years needed to detect a trend of 1 day/year increase in the cutting
# duration

# We estimte the process+sampling SD from the previous fit
Process.SD  <- summary(grass.fit)$sigma
Sampling.SD <- 0
Trend       <- 1

Xvalues <- 1:20 # 20 years of yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


Xvalues <- 1:30 # 30 years of yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


Xvalues <- seq(1,30,2) # 30 years of bi-yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


# (b) a proportion change (percentage change) per year
# We sometimes are interested in a proportional (percentage) change per year. For example, how many
# year do we need to sample to detect a 1% increase/year. We take advantage of the special form of the
# natural log, where a value of .01 on the log-scale corresponds to a (very close) approximation to a 1%
# increase per year.

# We need to convert the process and sampling errors to relative standard deviations by either
# repeating the analysis on the log-scale, or dividing the absolute standard deviatons by a typical,
# usually the mean, Y value. Both should give similar answers as shown below.

Process.SD.log1 <- summary(lm(log(days) ~ year, data=grass))$sigma
Process.SD.log2 <- summary(lm(days ~ year, data=grass))$sigma / mean(grass$days, na.rm=TRUE)
Sampling.SD.log <- 0 
cat("Process SD from log-analysis ", Process.SD.log1, ";  Process SD from relative SD ", Process.SD.log2, "\n")

# We should also conver our absolute trend to a proportional trend
Trend.prop <- Trend / mean(grass$days, na.rm=TRUE)

Xvalues <- 1:20 # 20 years of yearly sampling
slr.power.stroup( Trend=Trend.prop, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD.log1, Sampling.SD=Sampling.SD.log, alpha=0.05)
# You will see that the power is very similar, as it must in the two approaches


