# Dissolved solids (mg/L) in the Cuyahoga River, Ohio.
# Fit seasonal models

# This the MK2b.txt data file from the Kendall program for USGS at 
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

sink('tss-cuyahoga-seasonal-R-010.txt', split=TRUE)
##***part010b;
# Read in the data and restructure it into the long format
tss <- read.table("tss-cuyahoga-seasonal.txt", header=TRUE, as.is=TRUE,
                 strip.white=TRUE)
tss$YearMonth <- tss$Year + (tss$Month-.5)/12
head(tss)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=tss, aes(x=YearMonth, y=TSS))+
  ggtitle("TSS levels over time")+
  xlab("Year")+ylab("TSS")+
  geom_point(size=4)+
  geom_line()
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='tss-cuyahoga-seasonal-R-prelimplot.png')



##########################################################
##***partseasadjb;
# Seasonal adjustment by subtracting the MEDIAN of each Month
# from the observed data
tss <- ddply(tss,"Month", function(x){
    # return the median of each Month
    x$median <- median(x$TSS, na.rm=TRUE)
    x$TSS.adjusted <- x$TSS - x$median
    x
})
head(tss, n=20)
##***partseasadje;

# display the seasonally adjusted values
xtabs(TSS.adjusted ~ Month+Year, data=tss, sparse=TRUE)


# make an initial plot of the seasonally adjusted data
##***partprelimplotsab;
plotprelimsa <- ggplot(data=tss, aes(x=YearMonth, y=TSS.adjusted))+
  ggtitle("Seasonally adjusted TSS levels over time")+
  xlab("Year")+ylab("Seasonally Adjusted TSS levels")+
  geom_point(size=4)+
  geom_line()+geom_hline(yintercept=0)
plotprelimsa
##***partprelimplotsae;
ggsave(plot=plotprelimsa, file='tss-cuyahoga-seasonal-R-prelimplotsa.png')

# regression on the seasonally adjusted values
TSS.sa.fit <- lm(TSS.adjusted ~ YearMonth, data=tss)
summary(TSS.sa.fit)


# Do a Durbin-Watson test o the seasonally adjusted fit
sink("tss-cuyahoga-seasonal-R-dwtestsa.txt", split=TRUE)
##***partdwtestsab;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(TSS.sa.fit) # from the car package
  dwtest(TSS.sa.fit) # from the lmtest package

##***partdwtestsae;
sink()



########################################################################
# Fit the model using an ANCOVA 


# Also make sure that Month is declared as a factor
tss$MonthF <- factor(tss$Month)

sink('tss-cuyahoga-seasonal-R-regfitsep.txt', split=TRUE)
##***partregfitsepb;
# Fit a separate line for each year
d_ply(tss, "Month", function(x){
  # fit a separate line for each Month
  cat("\n\n***Separate fit for Month :", as.character(x$Month[1]),"\n")
  fit <- lm( TSS ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
})
##***partregfitsepe;
sink()




sink('tss-cuyahoga-seasonal-R-regfitnp.txt', split=TRUE)
##***partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that Month has been declared as a factor.
tss.fit.np <- lm( TSS ~ MonthF + Year + Year:MonthF, data=tss)
anova(tss.fit.np)
##***partregfitnpe;
sink()

sink('tss-cuyahoga-seasonal-R-regfitp.txt', split=TRUE)
##***partregfitpb;
# Fit the regression line with parallel slopes. Specify the Month term last
# to get the proper test for Month effects
# Be sure that Month has been declared as a factor.
# Because R fits Type I tests, specify the Year variable last in the model
tss.fit.p <- lm( TSS ~ MonthF + Year, data=tss)
summary(tss.fit.p)
anova(tss.fit.p)
##***partregfitpe;
sink()

sink("tss-cuyahoga-seasonal-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Month is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Month effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(tss.fit.p)
coef(tss.fit.p)
sqrt(diag(vcov(tss.fit.p))) # gives the SE
confint(tss.fit.p)
names(summary(tss.fit.p))
summary(tss.fit.p)$r.squared
summary(tss.fit.p)$sigma
##***partfitpiecese;
sink()


##***partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(tss.fit.p)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='tss-cuyahoga-seasonal-R-diagplot.png')


sink("tss-cuyahoga-seasonal-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(tss.fit.p) # from the car package
  dwtest(tss.fit.p) # from the lmtest package

##***partdwteste;
sink()

sink('tss-cuyahoga-seasonal-R-lsmeans.txt', split=TRUE)
##***partlsmeansb;
# Estimate the size of the Month effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
tss.fit.p.lsmo <- lsmeans::lsmeans(tss.fit.p, ~MonthF)
cld(tss.fit.p.lsmo)
##***partlsmeanse;
sink()


# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



sink('tss-cuyahoga-seasonal-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(tss$Year,na.rm=TRUE),
                                 max(tss$Year,na.rm=TRUE),1), 
                        MonthF=unique(tss$MonthF))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('tss-cuyahoga-seasonal-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(tss.fit.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=tss, aes(x=YearMonth, y=TSS))+
  ggtitle("TSS levels over time ")+
  xlab("Year")+ylab("TSS levels")+
  geom_point(size=4)+
  geom_line() + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit, group=MonthF))
plotfit
# Add the confidence intervals to the plot
##***partpredavge;
sink()

ggsave(plot=plotfit, file='tss-cuyahoga-seasonal-R-plotfit.png')


sink('tss-cuyahoga-seasonal-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(tss.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
##***partpredindive;
sink()



##################################################################
########################################################################
########################################################################
########################################################################
# Fitting a seasonal model using sin/cos terms


sink('tss-cuyahoga-seasonal-R-makesin.txt', split=TRUE)
##***partmakesinb;
# Create the sin/cosine terms
tss$cos <- cos(2*pi*tss$YearMonth/1)
tss$sin <- sin(2*pi*tss$YearMonth/1)
head(tss)
##***partmakesine;
sink()



sink('tss-cuyahoga-seasonal-R-regfitsin.txt', split=TRUE)
##***partregfitsinb;
# Fit the regression line with the sin/cos term
# Because lm() produces type I (increment tests), you need to specify the
# year term last in the model.
# Be sure that Month has been declared as a factor.
tss.fit.sin <- lm( TSS ~ sin + cos + Year , data=tss)
drop1(tss.fit.sin, test="F")
##***partregfitsine;
sink()


# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(tss.fit.sin)
plotdiag

ggsave(plot=plotdiag, file='tss-cuyahoga-seasonal-R-plotdiag.png')


sink('tss-cuyahoga-seasonal-R-regfitsin2.txt', split=TRUE)
##***partregfitsin2b;
# Fit the regression line with the sin/cos term
# Because lm() produces type I (increment tests), you need to specify the
# year term last in the model.
# Be sure that Month has been declared as a factor.
tss.fit.sin <- lm( TSS ~ sin + cos + YearMonth , data=tss)
#drop1(tss.fit.sin2, test="F")
summary(tss.fit.sin)
##***partregfitsin2e;
sink()



sink("tss-cuyahoga-seasonal-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Month is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Month effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(tss.fit.sin)
coef(tss.fit.sin)
sqrt(diag(vcov(tss.fit.sin))) # gives the SE
confint(tss.fit.sin)
names(summary(tss.fit.sin))
summary(tss.fit.sin)$r.squared
summary(tss.fit.sin)$sigma
##***partfitpiecese;
sink()



##***partdiagplot2b;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag2 <- sf.autoplot.lm(tss.fit.sin)
plotdiag2
##***partdiagplot2e;

ggsave(plot=plotdiag2, file='tss-cuyahoga-seasonal-R-diagplot2.png')




sink("tss-cuyahoga-seasonal-R-dwtest2.txt", split=TRUE)
##***partdwtest2b;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(tss.fit.sin) # from the car package
  dwtest(tss.fit.sin) # from the lmtest package

##***partdwtest2e;
sink()



sink('tss-cuyahoga-seasonal-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(YearMonth=seq(min(tss$Year,na.rm=TRUE),
                                 max(tss$Year,na.rm=TRUE)+1,.1)) 
newYears$cos <- cos(2*pi*newYears$YearMonth/1)
newYears$sin <- sin(2*pi*newYears$YearMonth/1)
newYears[1:5,]
head(newYears)
##***partpredsetupe;
sink()

sink('tss-cuyahoga-seasonal-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(tss.fit.sin, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=tss, aes(x=YearMonth, y=TSS))+
  ggtitle("TSS levels over time")+
  xlab("Year")+ylab("TSS levels")+
  geom_point(size=4)+
  geom_line() + 
  geom_line(data=predict.avg.df, aes(x=YearMonth, y=fit), color="red")
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, 
                aes(x=YearMonth,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()


ggsave(plot=plotfit, file='tss-cuyahoga-seasonal-R-plotfit-sin2.png')
ggsave(plot=plotfit.avgci, file='tss-cuyahoga-seasonal-R-plotpredavg-sin2.png')


sink('tss-cuyahoga-seasonal-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(tss.fit.sin, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, 
                aes(x=YearMonth,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='tss-cuyahoga-seasonal-R-plotpredindiv-sin2.png')




###################################################################################
# Seasonal Kendall test for trend. Need to do this for EACH Month.
# You cannot test for parallelism of trends in Months using Kendall's tau
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case


# Sort the data by year and Month
tss <- tss[order(tss$Year, tss$Month),]

# Compute Kendall's tau for each Month separately
Month.kendall <- ddply(tss,"Month", function(x){
  # Apply kendalls method to each Month
  #cat("\n\n***** Kendalls tau for Month: ", as.character(x$Month[1]),"\n")
  tau.test <- MannKendall(x$TSS)
  #browser()
  #print(summary(tau.test))
  # Notice that the test are invariant to transformations
  tau.test.log <- MannKendall(x$TSS)
  #print(tau.test.log)
  # compute the exact p0value
  tau.test3 <- cor.test( na.omit(x$TSS),1:length(na.omit(x$TSS)),
                        method= "kendall")
  res<- c(unlist(tau.test), tau.test3$p.value)
  names(res)[2] <- 'Approx.pvalue'
  names(res)[length(res)] <- 'Exact.pvalue'
  return(res)
})
Month.kendall

# Convert the Exact p-value to a zscore
Month.kendall$zscore <- abs(qnorm(Month.kendall$Exact.pvalue/2))*sign(Month.kendall$tau)

# Find the total z-score and a pvalue
totalz <- sum(Month.kendall$zscore)
total.pvalue <- pnorm(-abs(totalz), sd=sqrt(12))*2
total.pvalue


# The Seasonal Mann Kendall test is available. The function assumes
# that the data are Monthly values and that every Month is present.
# Consequently, you should check this and pad with NAs as needed

# We notice that there are lots of missing values in the dataset
xtabs(~Year+Month, data=tss)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(Year=seq(min(tss$Year, na.rm=TRUE),
                             max(tss$Year, na.rm=TRUE),1),
                    Month=1:12)
tss.full <- merge(tss, full, all=TRUE )
xtabs(~Year+Month, data=tss.full)
xtabs(TSS~Year+Month, data=tss.full, sparse=TRUE)

# order the data by year and Month
tss.full <- tss.full[order(tss.full$Year, tss.full$Month),]

# Do the SeaonalKendall after converting to a time-series object
tss.ts <- ts(tss.full$TSS, frequency=12) 
tss.ts
frequency(tss.ts)

sink('tss-cuyahoga-seasonal-R-seasonalMK.txt',split=TRUE)
##***partseasonalMKb;
SMK <- SeasonalMannKendall(tss.ts)
summary(SMK)
##***partseasonalMKe;
sink()

#########################################################################
# Kendall seasonal test for trend using the rkt package
# There are several packages: wq, Kendall, pheno and rkt, envstats
# See: http://www.researchgate.net/post/Is_there_a_seasonal_Kendall_test_script_for_R_or_RStudio

library(rkt)
rkt::rkt(tss$Year, tss$TSS, block=tss$Month )

#########################################################################
# Kendall seasonal test for trend using the EnvStats package
# There are several packages: wq, Kendall, pheno and rkt, envstats
# See: http://www.inside-r.org/node/218892

library(EnvStats)
EnvStats::kendallSeasonalTrendTest(TSS ~ Month + Year, data=tss)


#########################################################################
# Kendall seasonal test for trend using the wq package
# We need to convert it first to a time series object but we need
# to insert missing values for the missing months etc

xtabs(~Year+Month, data=tss)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(Year=seq(min(tss$Year, na.rm=TRUE),
                             max(tss$Year, na.rm=TRUE),1),
                    Month=1:12)
tss.full <- merge(tss, full, all=TRUE )
xtabs(~Year+Month, data=tss.full)
xtabs(TSS~Year+Month, data=tss.full, sparse=TRUE)

# order the data by year and Month
tss.full <- tss.full[order(tss.full$Year, tss.full$Month),]

tss.ts <- ts(tss.full$TSS, frequency=12) 
tss.ts
frequency(tss.ts)

library(wq)
wq::trendHomog(tss.ts) # test if the slopes are the same in all seasons
wq::seaKen(tss.ts)  # estimate the common slope 
wq::seasonTrend(tss.ts)
