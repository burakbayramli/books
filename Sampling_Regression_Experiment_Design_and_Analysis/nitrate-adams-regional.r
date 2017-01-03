# Nitrate (mg/L) in ground water, Adams County, Washington
# Fit regional kendall models

# This the RK3a.txt data file from the Kendall program for USGS at 
#    http://pubs.usgs.gov/sir/2005/5275/pdf/sir2005-5275.pdf

# 2015-06-27 CJS First edition

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(lsmeans)
library(nlme)
library(plyr)
library(reshape2)

source("../../schwarz.functions.r")

sink('nitrate-adams-regional-R-010.txt', split=TRUE)
##***part010b;
# Read in the data and restructure it into the long format
nitrate <- read.table("nitrate-adams-regional.txt", header=TRUE, as.is=TRUE,
                 strip.white=TRUE)
nitrate$RegionF <- factor(nitrate$Region) # make region a factor
head(nitrate)
##***part010e;
sink()

xtabs(~Year+Region, data=nitrate)

# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=nitrate, aes(x=Year, y=Nitrate, group=RegionF))+
  ggtitle("Nitrate levels over time")+
  xlab("Year")+ylab("Nitrate")+
  geom_point(size=4, position=position_dodge(w=0.2))+
  geom_line()
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='nitrate-adams-regional-R-prelimplot.png')




########################################################################
# Fit the model using an ANCOVA 


# Also make sure that Region is declared as a factor (see when data read in)

sink('nitrate-adams-regional-R-regfitsep.txt', split=TRUE)
##***partregfitsepb;
# Fit a separate line for each Region
d_ply(nitrate, "Region", function(x){
  # fit a separate line for each Region
  cat("\n\n***Separate fit for Region :", as.character(x$Region[1]),"\n")
  fit <- lm( Nitrate ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
})
##***partregfitsepe;
sink()




sink('nitrate-adams-regional-R-regfitnp.txt', split=TRUE)
##***partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that Region has been declared as a factor.
nitrate.fit.np <- lm( Nitrate ~ RegionF + Year + Year:RegionF, data=nitrate)
anova(nitrate.fit.np)
##***partregfitnpe;
sink()

sink('nitrate-adams-regional-R-regfitp.txt', split=TRUE)
##***partregfitpb;
# Fit the regression line with parallel slopes. Specify the Region term last
# to get the proper test for Region effects
# Be sure that Region has been declared as a factor.
# Because R fits Type I tests, specify the Year variable last in the model
nitrate.fit.p <- lm( Nitrate ~ RegionF + Year, data=nitrate)
summary(nitrate.fit.p)
anova(nitrate.fit.p)
##***partregfitpe;
sink()

sink("nitrate-adams-regional-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Region is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Region effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(nitrate.fit.p)
coef(nitrate.fit.p)
sqrt(diag(vcov(nitrate.fit.p))) # gives the SE
confint(nitrate.fit.p)
names(summary(nitrate.fit.p))
summary(nitrate.fit.p)$r.squared
summary(nitrate.fit.p)$sigma
##***partfitpiecese;
sink()


##***partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(nitrate.fit.p)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='nitrate-adams-regional-R-diagplot.png')


sink('nitrate-adams-regional-R-lsmeans.txt', split=TRUE)
##***partlsmeansb;
# Estimate the size of the Region effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
nitrate.fit.p.lsmo <- lsmeans::lsmeans(nitrate.fit.p, ~RegionF)
cld(nitrate.fit.p.lsmo)
##***partlsmeanse;
sink()


# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



sink('nitrate-adams-regional-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(nitrate$Year,na.rm=TRUE),
                                 max(nitrate$Year,na.rm=TRUE),1), 
                        RegionF=unique(nitrate$RegionF))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('nitrate-adams-regional-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(nitrate.fit.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=nitrate, aes(x=Year, y=Nitrate, group=RegionF))+
  ggtitle("Nitrate levels over time ")+
  xlab("Year")+ylab("Nitrate levels")+
  geom_point(size=4,position=position_dodge(w=0.2))+
  geom_line(data=predict.avg.df, aes(x=Year, y=fit, group=RegionF))
plotfit
# Add the confidence intervals to the plot
##***partpredavge;
sink()

ggsave(plot=plotfit, file='nitrate-adams-regional-R-plotfit.png')


sink('nitrate-adams-regional-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(nitrate.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
##***partpredindive;
sink()






#########################################################################
# Regional Kendall test for trend using the rkt package

library(rkt)
rkt::rkt(nitrate$Year, nitrate$Nitrate, block=nitrate$Region )

#########################################################################
# Regional Kendall test for trend using the EnvStats package
# There are several packages: wq, Kendall, pheno and rkt, envstats
# See: http://www.inside-r.org/node/218892

library(EnvStats)
EnvStats::kendallSeasonalTrendTest(Nitrate ~ Region + Year, data=nitrate)


#########################################################################
# Kendall regional test for trend using the wq package
# We need to convert it first to a matrix time series object with missing values for any 
# years not measured in a region

xtabs(~Year+Region, data=nitrate)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(Year=seq(min(nitrate$Year, na.rm=TRUE),
                             max(nitrate$Year, na.rm=TRUE),1),
                    Region=unique(nitrate$Region))
nitrate.full <- merge(nitrate, full, all=TRUE )
xtabs(~Year+Region, data=nitrate.full)
xtabs(Nitrate~Year+Region, data=nitrate.full, sparse=TRUE)

# Convert to a matrix time series
# Convert from long to wide format
library(reshape2)
nitrate.wide <- acast(nitrate.full, Year ~ Region, value.var="Nitrate")
nitrate.wide
nitrate.mts <- ts(nitrate.wide, class=c("ts","mts"))
frequency(nitrate.mts)

library(wq)
wq::trendHomog(mts2ts(nitrate.mts)) # test if the slopes are the same in all regions
wq::seaKen(mts2ts(nitrate.mts))  # regional test
