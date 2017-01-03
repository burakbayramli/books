# Ammonium  (uEq/L) in snowpack in Colorado and New Mexico.
# Fit regional kendall models

# This the RK3b.txt data file from the Kendall program for USGS at 
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

sink('ammonium-regional-R-010.txt', split=TRUE)
##***part010b;
# Read in the data and restructure it into the long format
ammon <- read.table("ammonium-regional.txt", header=TRUE, as.is=TRUE,
                 strip.white=TRUE)
ammon$RegionF <- factor(ammon$Region) # make region a factor
head(ammon)
##***part010e;
sink()

xtabs(~Year+Region, data=ammon)

# make an initial plot of the data
##***partprelimplotb;
library(scales)
plotprelim <- ggplot(data=ammon, aes(x=Year, y=Ammonium, group=RegionF))+
  ggtitle("Ammonium levels over time")+
  xlab("Year")+ylab("Ammonium")+
  geom_point(size=4, position=position_dodge(w=0.2))+
  geom_line()+
  scale_x_continuous(breaks=pretty_breaks())
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='ammonium-regional-R-prelimplot.png')




########################################################################
# Fit the model using an ANCOVA 


# Also make sure that Region is declared as a factor (see when data read in)

sink('ammonium-regional-R-regfitsep.txt', split=TRUE)
##***partregfitsepb;
# Fit a separate line for each Region
ddply(ammon, "Region", function(x){
  # fit a separate line for each Region
  cat("\n\n***Separate fit for Region :", as.character(x$Region[1]),"\n")
  fit <- lm( Ammonium ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
  res <- data.frame(slope=coef(fit)[2], se=sqrt(diag(vcov(fit)))[2])
  return(res)
})
##***partregfitsepe;
sink()




sink('ammonium-regional-R-regfitnp.txt', split=TRUE)
##***partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that Region has been declared as a factor.
ammon.fit.np <- lm( Ammonium ~ RegionF + Year + Year:RegionF, data=ammon)
anova(ammon.fit.np)
##***partregfitnpe;
sink()

sink('ammonium-regional-R-regfitp.txt', split=TRUE)
##***partregfitpb;
# Fit the regression line with parallel slopes. Specify the Region term last
# to get the proper test for Region effects
# Be sure that Region has been declared as a factor.
# Because R fits Type I tests, specify the Year variable last in the model
ammon.fit.p <- lm( Ammonium ~ RegionF + Year, data=ammon)
summary(ammon.fit.p)
anova(ammon.fit.p)
##***partregfitpe;
sink()

sink("ammonium-regional-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Region is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Region effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(ammon.fit.p)
coef(ammon.fit.p)
sqrt(diag(vcov(ammon.fit.p))) # gives the SE
confint(ammon.fit.p)
names(summary(ammon.fit.p))
summary(ammon.fit.p)$r.squared
summary(ammon.fit.p)$sigma
##***partfitpiecese;
sink()


##***partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(ammon.fit.p)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='ammonium-regional-R-diagplot.png')


sink('ammonium-regional-R-lsmeans.txt', split=TRUE)
##***partlsmeansb;
# Estimate the size of the Region effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
ammon.fit.p.lsmo <- lsmeans::lsmeans(ammon.fit.p, ~RegionF)
cld(ammon.fit.p.lsmo)
##***partlsmeanse;
sink()


# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



sink('ammonium-regional-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(ammon$Year,na.rm=TRUE),
                                 max(ammon$Year,na.rm=TRUE),1), 
                        RegionF=unique(ammon$RegionF))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('ammonium-regional-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(ammon.fit.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=ammon, aes(x=Year, y=Ammonium, group=RegionF))+
  ggtitle("Ammonium levels over time ")+
  xlab("Year")+ylab("Ammonium levels")+
  geom_point(size=4,position=position_dodge(w=0.2))+
  geom_line(data=predict.avg.df, aes(x=Year, y=fit, group=RegionF))
plotfit
# Add the confidence intervals to the plot
##***partpredavge;
sink()

ggsave(plot=plotfit, file='ammonium-regional-R-plotfit.png')


sink('ammonium-regional-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(ammon.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
##***partpredindive;
sink()






#########################################################################
# Regional Kendall test for trend using the rkt package

library(rkt)
rkt::rkt(ammon$Year, ammon$Ammonium, block=ammon$Region )

#########################################################################
# Regional Kendall test for trend using the EnvStats package
# There are several packages: wq, Kendall, pheno and rkt, envstats
# See: http://www.inside-r.org/node/218892

library(EnvStats)
EnvStats::kendallSeasonalTrendTest(Ammonium ~ Region + Year, data=ammon)


#########################################################################
# Kendall regional test for trend using the wq package
# We need to convert it first to a matrix time series object with missing values for any 
# years not measured in a region

xtabs(~Year+Region, data=ammon)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(Year=seq(min(ammon$Year, na.rm=TRUE),
                             max(ammon$Year, na.rm=TRUE),1),
                    Region=unique(ammon$Region))
ammon.full <- merge(ammon, full, all=TRUE )
xtabs(~Year+Region, data=ammon.full)
xtabs(Ammonium~Year+Region, data=ammon.full, sparse=TRUE)

# Convert to a matrix time series
# Convert from long to wide format
library(reshape2)
ammon.wide <- acast(ammon.full, Year ~ Region, value.var="Ammonium")
ammon.wide
ammon.mts <- ts(ammon.wide, class=c("ts","mts"))
frequency(ammon.mts)

library(wq)
wq::trendHomog(mts2ts(ammon.mts)) # test if the slopes are the same in all regions
wq::seaKen(mts2ts(ammon.mts))  # regional test
