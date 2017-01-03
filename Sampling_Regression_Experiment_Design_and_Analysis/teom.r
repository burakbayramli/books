# TEOM vs. Reference meter for PM 2.5 measurements.
# 2014-05-04 CJS first edition

# The air that we breath often has many contaminants. One contaminant of 
# interest is {\bf Particulate Matter (PM)}.
# Particulate matter is the general term used for a mixture of solid 
# particles and liquid droplets in the air. It includes aerosols, smoke, 
# fumes, dust, ash and pollen. 
# The composition of particulate matter varies with place, season and 
# weather conditions.

# Particulate matter is characterized according to size - mainly because of 
# the different health effects associated with particles of different diameters.
# Fine particulate matter is particulate matter that is 2.5 microns in 
# diameter and less. [A human hair is approximately 30 times larger than 
# these particles! The smaller particles are so small that several thousand 
# of them could fit #on the period at the end of this sentence.
# It is also known
# as PM2.5 or respirable particles because it penetrates the respiratory 
# system further than larger particles.

# PM2.5 material is primarily formed from chemical reactions in the 
# atmosphere and through fuel combustion (e.g., motor vehicles, 
# power generation, industrial facilities residential fire places, wood 
# stoves and agricultural burning). Significant amounts of
# PM2.5 are carried into Ontario from the U.S. During periods of widespread 
# elevated levels of fine particulate matter, it is
# estimated that more than 50 per cent of Ontario's PM2.5 comes from the U.S.

# Adverse health effects from breathing air with a high PM 2.5 concentration 
# include: premature death, increased respiratory
# symptoms and disease, chronic bronchitis, and decreased lung function 
# particularly for individuals with asthma.

# Further information about fine particulates is available at many websites 
#   \url{http://www.health.state.ny.us/nysdoh/indoor/pmq_a.htm} and 
#   \url{http://www.airqualityontario.com/science/pollutants/#particulates.cfm}, and
#   \url{http://www.epa.gov/pmdesignations/faq.htm}.

# The PM2.5 concentrations in air can be measured in many ways. A well known 
# method is a is a filter based method whereby one 24 hour sample is 
# collected every third day.
# The sampler draws air through a pre-weighed filter for a specified period 
# (usually 24 hours) at a known flowrate. The
# filter is then removed and sent to a laboratory to determine the gain in 
# filter mass due to particle collection. Ambient PM
# concentration is calculated on the basis of the gain in filter mass, 
# divided by the product of sampling period and sampling
# flowrate. Additional analysis can also be performed on the filter to 
# determine the chemical composition of the sample.

# In recent years, a program of continuous sampling using automatic samplers 
# has been introduced. An
# instrument widely adopted for this use is the Tapered Element Oscillating 
# Microbalance (TEOM). The TEOM operates under the
# following principles. Ambient air is drawn in through a heated inlet. It 
# is then drawn through a filtered cartridge on the end
# of a hollow, tapered tube. The tube is clamped at one end and oscillates 
# freely like a tuning fork. As particulate matter
# gathers on the filter cartridge, the natural frequency of oscillation of 
# the tube decreases. The mass accumulation of
# particulate matter is then determined from the corresponding change in 
# frequency. 

# Because of the different ways in which these instruments work, a 
# calibration experiment was performed
# The hourly TEOM readings were accumated to a daily value and compared to 
# those obtained
# from an air filter method.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(lsmeans)
library(plyr)
library(reshape2)

source("../../schwarz.functions.r")

sink('teom-R-010.txt', split=TRUE)
##---part010b;
# Read in the data and deal with the date variables
teom <- read.csv("teom.csv", header=TRUE, as.is=TRUE,
                 strip.white=TRUE, na.string=".")
teom$Date <- as.Date(teom$Date, "%Y.%m.%d")
teom$Year <- as.numeric((teom$Date-as.Date("2003-01-01"))/365.25)
teom$logRatio <- log(teom$TEOM/teom$Reference)
teom$cos    <- cos(2*pi*teom$Year/1)
teom$sin    <- sin(2*pi*teom$Year/1)
str(teom)
head(teom)
##---part010e;
sink()


# make an initial plot of the data
##---partprelimplotb;
plotprelim <- ggplot(data=teom, aes(x=Year, y=logRatio))+
  ggtitle("log Ratio(TEOM / Reference) over time")+
  xlab("Year")+ylab("log Ratio(TEOM / Reference)")+
  geom_point(size=4)+
  geom_line()
plotprelim
##---partprelimplote;

ggsave(plot=plotprelim, file='teom-R-prelimplot.png')

# Using Base R graphics
with(teom, plot(Year, logRatio, type="b", 
    main='log Ratio(TEOM / Reference) over time',
    xlab='Year',ylab="log Ratio(TEOM / Reference)")  )


# Fitting a seasonal model using sin/cos terms


sink('teom-R-regfitsin.txt', split=TRUE)
##---partregfitsinb;
# Fit the regression line with the sin/cos term
# Because lm() produces type I (increment tests), you need to specify the
# year term last in the model.
# Be sure that month has been declared as a factor.
teom.fit.sin <- lm( logRatio ~ sin + cos + Year , data=teom)
drop1(teom.fit.sin, test="F")
summary(teom.fit.sin)
##---partregfitsine;
sink()


# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(teom.fit.sin)
plotdiag

ggsave(plot=plotdiag, file='teom-R-diagplot.png')



sink('teom-R-regfitsin2.txt', split=TRUE)
##---partregfitsin2b;
# Fit the regression line with the sin/cos term but dropping the year term
teom.fit.sin2 <- lm( logRatio ~ sin + cos  , data=teom)
drop1(teom.fit.sin2, test="F")
summary(teom.fit.sin2)
##---partregfitsin2e;
sink()



sink("teom-R-fitpieces.txt", split=TRUE)
##---partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because month is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the month effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(teom.fit.sin2)
coef(teom.fit.sin2)
sqrt(diag(vcov(teom.fit.sin2))) # gives the SE
confint(teom.fit.sin2)
names(summary(teom.fit.sin2))
summary(teom.fit.sin2)$r.squared
summary(teom.fit.sin2)$sigma
##---partfitpiecese;
sink()



##---partdiagplot2b;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag2 <- sf.autoplot.lm(teom.fit.sin2)
plotdiag2
##---partdiagplot2e;

ggsave(plot=plotdiag2, file='teom-R-diagplot2.png')

# Get the diagnostic plots using Base R graphics
layout(matrix(1:4,2,2))
plot(teom.fit.sin2)
layout(1)


sink("teom-R-dwtest2.txt", split=TRUE)
##---partdwtest2b;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(teom.fit.sin2) # from the car package
  dwtest(teom.fit.sin2) # from the lmtest package

##---partdwtest2e;
sink()



sink('teom-R-predsetup.txt', split=TRUE)
##---partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(Year=seq(min(teom$Year,na.rm=TRUE),
                                 max(teom$Year,na.rm=TRUE)+1,.1)) 
newYears$cos <- cos(2*pi*newYears$Year/1)
newYears$sin <- sin(2*pi*newYears$Year/1)
head(newYears)
##---partpredsetupe;
sink()

sink('teom-R-predavg.txt', split=TRUE)
##---partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(teom.fit.sin2, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=teom, aes(x=Year, y=logRatio))+
  ggtitle("logRatiophorus levels over time - outliers removed")+
  xlab("Year")+ylab("logRatiophorus levels (mg/L)")+
  geom_point(size=4)+
  geom_line() + 
  geom_line(data=predict.avg.df, aes(x=Year, y=fit), color="red")
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, 
                aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##---partpredavge;
sink()


ggsave(plot=plotfit, file='teom-R-plotfit-sin2.png')
ggsave(plot=plotfit.avgci, file='teom-R-plotpredavg-sin2.png')

# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to specify the y axis limits in advance of the plot
with(teom, plot(Year, logRatio,
    main='log Ratio(TEOM / Reference) over time',
    xlab='Year',ylab="log Ratio(TEOM / Reference)",
    ylim=range(c(teom$logRatio, predict.avg.df$lwr  , predict.avg.df$upr), na.rm=TRUE))
    )  
  with(predict.avg.df, lines(Year, fit))
  with(predict.avg.df, lines(Year, lwr, lty=2))
  with(predict.avg.df, lines(Year, upr, lty=2))



sink('teom-R-predindiv.txt', split=TRUE)
##---partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(teom.fit.sin2, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, 
                aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##---partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='teom-R-plotpredindiv-sin2.png')


# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to reset the plotting limits IN ADVANCE
with(teom, plot(Year, logRatio,type="b",
    main='log Ratio(TEOM / Reference) over time',
    xlab='Year',ylab="log Ratio(TEOM / Reference) (mg/L)",
    ylim=range(c(teom$logRatio, predict.avg.df$lwr  , predict.avg.df$upr,
                       predict.indiv.df$lwr, predict.indiv.df$upr), na.rm=TRUE))
    )
  with(predict.indiv.df, lines(Year, fit))
  with(predict.indiv.df, lines(Year, lwr, lty=2))
  with(predict.indiv.df, lines(Year, upr, lty=2))
  with(predict.indiv.df, lines(Year, lwr, lty=3, col="green"))
  with(predict.indiv.df, lines(Year, upr, lty=3, col="green"))

