# Dissolved solids (mg/L) in the Cuyahoga River, Ohio.
# This the MK4a.txt data file from the Kendall program for USGS at 
#    http://pubs.usgs.gov/sir/2005/5275/pdf/sir2005-5275.pdf

# 2015-06-27 CJS First edition


options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(randtests)
library(zyp)

source("../../schwarz.functions.r")


# Read in the data.Note that some Years are missing the time of the breakup
sink('cuyahoga-R-010.txt', split=TRUE)
##***part010b;
tss <- read.table("tss-cuyahoga.txt", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string="NA")
head(tss)
str(tss)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=tss, aes(x=Year, y=TSS))+
  ggtitle("Cuyahoga River TSS")+
  xlab("Year")+ylab("TSS")+
  geom_point()+
  geom_line()
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='cuyahoga-R-prelimplot.png')


# Fit the regression line and get the results
sink('cuyahoga-R-regfit.txt', split=TRUE)
##***partregfitb;
tss.fit <- lm( TSS ~ Year, data=tss)
summary(tss.fit)
##***partregfite;
sink()

sink("cuyahoga-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(tss.fit)
coef(tss.fit)
sqrt(diag(vcov(tss.fit))) # gives the SE
confint(tss.fit)
names(summary(tss.fit))
summary(tss.fit)$r.squared
summary(tss.fit)$sigma
##***partfitpiecese;
sink()

##***partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(tss.fit)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='cuyahoga-R-diagplot.png')


sink("cuyahoga-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

durbinWatsonTest(tss.fit) # from the car package
dwtest(tss.fit) # from the lmtest package
##***partdwteste;
sink()

##***partplotfitb;
# plot the fitted line to the graphs
plotfit <- plotprelim + 
     geom_abline(intercept=coef(tss.fit)[1], slope=coef(tss.fit)[2])
plotfit
##***partplotfite;

ggsave(plot=plotfit, file='cuyahoga-R-plotfit.png')

# or we can use the builtin smoothing functions of ggplot which also gives 
# a confidence interval for the MEAN response
plotfit2 <- plotprelim +
     geom_smooth(method="lm")
plotfit2


sink('cuyahoga-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(Year=seq(min(tss$Year,na.rm=TRUE),3+max(tss$Year,na.rm=TRUE),1))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('cuyahoga-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE time of breakup for each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(tss.fit, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look ntss
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()

ggsave(plot=plotfit.avgci, file='cuyahoga-R-plotpredavg.png')


sink('cuyahoga-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL time of breakup at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(tss.fit, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look ntss
predict.indiv.df <- cbind(newYears, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='cuyahoga-R-plotpredindiv.png')


# No easy way to do inverse predictions


###################################################################################
# Cox and Stuart Test for trend
# This is simply a sign.test applied to the paired data
# It is available in the randtests package
# You must make sure that the data is sorted by time ordering and that all missing
# values are removed

sink("cuyahoga-R-coxstuart.txt",split=TRUE)
##***partcoxstuartb;
tss <- tss[order(tss$Year),]
TSS.no.missing <- tss$TSS[!is.na(tss$TSS)]
cox.stuart.test(TSS.no.missing)
##***partcoxstuarte;
sink()


##############################################################################
# Spearman's rho (non-parametric correlation)

sink('cuyahoga-R-spearman.txt',split=TRUE)
##***partspearmanb;
# Because there is no distiction between the response and predictor variables,
# the formula is specified with both variables to the right
# of the tilde
cor.test( ~TSS + Year, data=tss, method="spearman")
##***partspearmane;
sink()

###################################################################################
# Kendall test for trend
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# The MannKendall test is found in the Kendall package
# or it can be computed using the cor.test() function as well

sink('cuyahoga-R-kendalltau.txt',split=TRUE)
##***partkendalltaub;
tau.test <- Kendall(tss$TSS, tss$Year)
summary(tau.test)
tau.test2 <-cor.test( ~TSS + Year, data=tss, method="kendall")
tau.test2
##***partkendalltaue;
sink()

sink('cuyahoga-R-senslope.txt',split=TRUE)
##***partsenslopeb;
# Now to estimate the slope using the Sen estimator in the zyp package
sen.slope <- zyp.sen(TSS~Year, data=tss)
sen.slope$coef
confint.zyp(sen.slope)
##***partsenslopee;
sink()



#########################################################################
# Kendall test for trend using the rkt package
# There are several packages: wq, Kendall, pheno and rkt, envstats
# See: http://www.researchgate.net/post/Is_there_a_seasonal_Kendall_test_script_for_R_or_RStudio

library(rkt)
rkt::rkt(tss$Year, tss$TSS)


#########################################################################
# Kendall test for trend using the EnvStats package

library(EnvStats)
EnvStats::kendallTrendTest(TSS ~ Year, data=tss)


#########################################################################
# Kendall test for trend using the wq package
# We need to convert it first to a time series object but we need
# to insert missing values for the missing months etc

tss$YearTrunc <- trunc(tss$Year)
tss$Month <- round(.6 + (tss$Year - trunc(tss$YearTrunc))*12)
xtabs(~YearTrunc+Month, data=tss)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(YearTrunc=seq(min(tss$YearTrunc, na.rm=TRUE),
                                  max(tss$YearTrunc, na.rm=TRUE),1),
                    Month=1:12)
tss.full <- merge(tss, full, all=TRUE )
xtabs(~YearTrunc+Month, data=tss.full)
xtabs(TSS~YearTrunc+Month, data=tss.full, sparse=TRUE)

# order the data by year and Month
tss.full <- tss.full[order(tss.full$YearTrunc, tss.full$Month),]

tss.ts <- ts(tss.full$TSS, frequency=12) 
tss.ts
frequency(tss.ts)

library(wq)
wq::mannKen(tss.ts, plot = FALSE, type = c("slope", "pct", "tau"), order = FALSE)
