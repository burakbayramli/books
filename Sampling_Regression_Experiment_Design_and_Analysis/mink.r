# Mink pelts in Saskatchewan
# Dealing with Autocorrelation in time series
# 2014-05-05 CJS First edition

# L.B. Keith (1963) collected information on the number of 
# mink-pelts from  Saskatchewan, Canada over a 30 year period.

# This is data series 3707 in the
# NERC Centre for Population Biology, Imperial College (1999)
# The Global Population Dynamics Database available at
# \url{http://www.sw.ic.ac.uk/cpb/cpb/gpdd.html}.


options(useFancyQuotes=FALSE) # renders summary output corrects


library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(lsmeans)
library(nlme)
library(plyr)

source("../../schwarz.functions.r")



# Read in the data. Declare Epoch as a factor. Remove data points when location changed in a year
sink('mink-R-010.txt', split=TRUE)
##---part010b;
mink <- read.csv("mink.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
mink$logPelts <- log(mink$Pelts)
head(mink)
str(mink)
##---part010e;
sink()

# make an initial plot of the data
# Notice how we specify a different plotting symbol for each Epoch.
##---partprelimplotb;
plotprelim <- ggplot(data=mink, aes(x=Year, y=logPelts))+
  ggtitle("Mink Pelts over time")+
  xlab("Year")+ylab("log(Mink Pelts)")+
  geom_point(size=4)+geom_line()
plotprelim
##---partprelimplote;

ggsave(plot=plotprelim, file='mink-R-prelimplot.png')

# Using Base R graphics
with(mink, plot(Year, logPelts, type="b",
    main='Mink Pelts over time',
    xlab='Year',ylab="log(Mink Pelts) ")  )




sink('mink-R-regfit.txt', split=TRUE)
##---partregfitb;
# Fit the regression line to the log(Pelts)
mink.fit <- lm( logPelts ~ Year, data=mink)
summary(mink.fit)
##---partregfite;
sink()

##---partresidlagb;
# Look at residual plot over time
resid <- data.frame(resid=resid(mink.fit),Year=mink$Year)
resid$lagresid <- c(NA, resid$resid[1:(length(resid$resid)-1)])
residtimeplot <- ggplot(data=resid, aes(x=Year, y=resid))+
  ggtitle("Time plot of residuals from a simple linear fit")+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=0)
residtimeplot
residlagplot <- ggplot(data=resid, aes(x=lagresid, y=resid))+
   ggtitle("Lag plot of residuals")+
     geom_point()
residlagplot
##---partresidlage;

ggsave(plot=residtimeplot, file='mink-R-residtimeplot.png')
ggsave(plot=residlagplot,  file='mink-R-residlagplot.png')

##---partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(mink.fit)
plotdiag
##---partdiagplote;

ggsave(plot=plotdiag, file='mink-R-diagplot.png')

# Get the diagnostic plots using Base R graphics
layout(matrix(1:4,2,2))
plot(mink.fit)
layout(1)


sink("mink-R-dwtest.txt", split=TRUE)
##---partdwtestb;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(mink.fit) # from the car package
  dwtest(mink.fit) # from the lmtest package
##---partdwteste;
sink()

sink('mink-R-ar1.txt', split=TRUE)
##---partar1b;
# Fit a model that allows for autocorrelation using gls
mink.fit.ar1 <- gls(logPelts ~ Year, data=mink,
                 correlation=corAR1(form=~1))
summary(mink.fit.ar1)
anova(mink.fit.ar1)

# Fit a model that allows for autocorrelation using ARIMA
mink.fit.arima <- with(mink, arima(logPelts, xreg=Year, order=c(1,0,0)))
mink.fit.arima
##---partar1e;
sink()

sink("mink-R-fitpieces.txt", split=TRUE)
##---partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Epoch is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Epoch effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(mink.fit.ar1)
coef(mink.fit.ar1)
sqrt(diag(vcov(mink.fit.ar1))) # gives the SE
confint(mink.fit.ar1)
names(summary(mink.fit.ar1))
summary(mink.fit.ar1)$r.squared
summary(mink.fit.ar1)$sigma
##---partfitpiecese;
sink()


# predictions are now much more complex because 
# of the autocorrelation. The general problem is that
# the autocorrelation structure implies that predictions at
# a particular year must incorporate the residuals from
# the previous years. Of course, this influence will diminish
# once you get further and further past the end of the series.
# 
# Contact me for details








