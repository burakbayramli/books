# Phosphorus levels on Klamath River.
# Testing for trend after seasonal adjustment
# 2014-05-04 CJS First edition

# Total phosphorus (mg/L) from NASQAN month 115305.00 on Klamath River, near
# Klamath Ca. The data was extracted from the NASQAN database and 
# modified to match  Figure 1 of Hirsch et al.
# This is the data used by Hirsch et al. (1982)


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
library(nlme)
library(plyr)
library(reshape2)

source("../../schwarz.functions.r")

sink('klamath-R-010.txt', split=TRUE)
##---part010b;
# Read in the data and restructure it into the long format
temp <- read.csv("klamath.csv", header=TRUE, as.is=TRUE,
                 strip.white=TRUE, na.string=".")
#temp
temp$X <- NULL
plevel <- melt(temp, id.var="month",
               measure.vars=names(temp)[grep('X',names(temp))],
               variable.name="XYear",
               value.name="phos")
plevel$Year      <- as.numeric(substr(as.character(plevel$XYear),2,5))
plevel$XYear     <- NULL
plevel$YearMonth <- plevel$Year + (plevel$month+.5)/12
str(plevel)
head(plevel)
##---part010e;
sink()


# make an initial plot of the data
##---partprelimplotb;
plotprelim <- ggplot(data=plevel, aes(x=YearMonth, y=phos))+
  ggtitle("Phosphorus levels over time")+
  xlab("Year")+ylab("Phosphorus levels (mg/L)")+
  geom_point(size=4)+
  geom_line()
plotprelim
##---partprelimplote;

ggsave(plot=plotprelim, file='klamath-R-prelimplot.png')

# Using Base R graphics
with(plevel, plot(YearMonth, phos, type="b", 
    main='Phosphorus levels over time',
    xlab='Year',ylab="Phosphorus levels (mg/L)")  )


##########################################################
##---partseasadjb;
# Seasonal adjustment by subtracting the MEDIAN of each month
# from the observed data
medians <- ddply(plevel,"month", function(x){
    # return the median of each month
    median <- median(x$phos, na.rm=TRUE)
    names(median) <- "monthly.median"
    median
})
medians

# Merge back with the original data and sort the values
plevel <- merge(plevel, medians)
plevel$seas.phos <- plevel$phos - plevel$monthly.median
plevel <- plevel[order(plevel$YearMonth),]
head(plevel)
##---partseasadje;

# display the seasonally adjusted values
xtabs(seas.phos ~ month+Year, data=plevel, sparse=TRUE)


# make an initial plot of the seasonally adjusted data
##---partprelimplotsab;
plotprelimsa <- ggplot(data=plevel, aes(x=YearMonth, y=seas.phos))+
  ggtitle("Seasonally adjusted Phosphorus levels over time")+
  xlab("Year")+ylab("Seasonally Adjusted Phosphorus levels (mg/L)")+
  geom_point(size=4)+
  geom_line()+geom_hline(yintercept=0)
plotprelimsa
##---partprelimplotsae;
ggsave(plot=plotprelimsa, file='klamath-R-prelimplotsa.png')

# regression on the seasonally adjusted values
phos.sa.fit <- lm(seas.phos ~ YearMonth, data=plevel)
summary(phos.sa.fit)

sink('klamath-R-regfitsa.txt', split=TRUE)
##---partregfitsab;
# regression analysis on the seasonally adjusted values
# after removing outliers
phos.sa.fit2 <- lm(seas.phos ~ YearMonth, data=plevel,
                  subset=plevel$seas.phos <= 0.2)
summary(phos.sa.fit2)
##---partregfitsae;
sink()

# Do a Durbin-Watson test o the seasonally adjusted fit
sink("klamath-R-dwtestsa.txt", split=TRUE)
##---partdwtestsab;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(phos.sa.fit2) # from the car package
  dwtest(phos.sa.fit2) # from the lmtest package

##---partdwtestsae;
sink()



########################################################################
# Fit the model using an ANCOVA 

# First remove all outlier more than 0.20
plevel <- plevel[ plevel$phos < 0.20,]

# Also make sure that month is declared as a factor
plevel$month <- factor(plevel$month)

sink('klamath-R-regfitsep.txt', split=TRUE)
##---partregfitsepb;
# Fit a separate line for each year
d_ply(plevel, "month", function(x){
  # fit a separate line for each month
  cat("\n\n***Separate fit for month :", as.character(x$month[1]),"\n")
  fit <- lm( phos ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
})
##---partregfitsepe;
sink()




sink('klamath-R-regfitnp.txt', split=TRUE)
##---partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that month has been declared as a factor.
plevel.fit.np <- lm( phos ~ month + Year + Year:month, data=plevel)
anova(plevel.fit.np)
##---partregfitnpe;
sink()

sink('klamath-R-regfitp.txt', split=TRUE)
##---partregfitpb;
# Fit the regression line with parallel slopes. Specify the month term last
# to get the proper test for month effects
# Be sure that month has been declared as a factor.
plevel.fit.sin2 <- lm( phos ~ Year + month, data=plevel)
summary(plevel.fit.sin2)
anova(plevel.fit.sin2)
##---partregfitpe;
sink()

sink("klamath-R-fitpieces.txt", split=TRUE)
##---partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because month is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the month effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(plevel.fit.sin2)
coef(plevel.fit.sin2)
sqrt(diag(vcov(plevel.fit.sin2))) # gives the SE
confint(plevel.fit.sin2)
names(summary(plevel.fit.sin2))
summary(plevel.fit.sin2)$r.squared
summary(plevel.fit.sin2)$sigma
##---partfitpiecese;
sink()


##---partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(plevel.fit.sin2)
plotdiag
##---partdiagplote;

ggsave(plot=plotdiag, file='klamath-R-diagplot.png')

# Get the diagnostic plots using Base R graphics
layout(matrix(1:4,2,2))
plot(plevel.fit.sin2)
layout(1)


sink("klamath-R-dwtest.txt", split=TRUE)
##---partdwtestb;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(plevel.fit.sin2) # from the car package
  dwtest(plevel.fit.sin2) # from the lmtest package

##---partdwteste;
sink()

sink('klamath-R-lsmeans.txt', split=TRUE)
##---partlsmeansb;
# Estimate the size of the month effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
plevel.fit.sin2.lsmo <- lsmeans(plevel.fit.sin2, ~month)
cld(plevel.fit.sin2.lsmo)
##---partlsmeanse;
sink()


# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



sink('klamath-R-predsetup.txt', split=TRUE)
##---partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(plevel$Year,na.rm=TRUE),
                                 max(plevel$Year,na.rm=TRUE),1), 
                        month=unique(plevel$month))
newYears[1:5,]
str(newYears)
##---partpredsetupe;
sink()

sink('klamath-R-predavg.txt', split=TRUE)
##---partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(plevel.fit.sin2, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=plevel, aes(x=YearMonth, y=phos))+
  ggtitle("Phosphorus levels over time - outliers removed")+
  xlab("Year")+ylab("Phosphorus levels (mg/L)")+
  geom_point(size=4)+
  geom_line() + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit, group=month))
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, 
                aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##---partpredavge;
sink()


ggsave(plot=plotfit, file='klamath-R-plotfit.png')
ggsave(plot=plotfit.avgci, file='klamath-R-plotpredavg.png')

# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to specify the y axis limits in advance of the plot
with(plevel, plot(YearMonth, phos,
    main='log(Phosphorus) over time',
    xlab='Year',ylab="log(Phosphorus) (mg/L)",
    ylim=range(c(plevel$phos, predict.avg.df$lwr  , predict.avg.df$upr), na.rm=TRUE))
    )  
d_ply(predict.avg.df, "month", function(x){
  with(x, lines(Year, fit))
  with(x, lines(Year, lwr, lty=2))
  with(x, lines(Year, upr, lty=2))
})

# You can plot everything back on the anti-logscale as well - try it!


sink('klamath-R-predindiv.txt', split=TRUE)
##---partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(plevel.fit.sin2, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##---partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='klamath-R-plotpredindiv.png')


# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to reset the plotting limits IN ADVANCE
with(plevel, plot(Year, phos,
    main='log(Phosphorus) over time',
    xlab='Year',ylab="log(Phosphorus) (mg/L)",
    ylim=range(c(plevel$phos, predict.avg.df$lwr  , predict.avg.df$upr,
                       predict.indiv.df$lwr, predict.indiv.df$upr), na.rm=TRUE))
    )
d_ply(predict.indiv.df, "month", function(x){
  with(x, lines(Year, fit))
  with(x, lines(Year, lwr, lty=2))
  with(x, lines(Year, upr, lty=2))
  with(x, lines(Year, lwr, lty=3, col="green"))
  with(x, lines(Year, upr, lty=3, col="green"))
})



# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended



########################################################################
########################################################################
########################################################################
########################################################################
# Fitting a seasonal model using sin/cos terms

# read in the data again
temp <- read.csv("klamath.csv", header=TRUE, as.is=TRUE,
                 strip.white=TRUE, na.string=".")
temp
temp$X <- NULL
plevel <- melt(temp, id.var="month",
               measure.vars=names(temp)[grep('X',names(temp))],
               variable.name="XYear",
               value.name="phos")
plevel$Year      <- as.numeric(substr(as.character(plevel$XYear),2,5))
plevel$XYear     <- NULL
plevel$YearMonth <- plevel$Year + (plevel$month+.5)/12
str(plevel)
head(plevel)


sink('klamath-R-makesin.txt', split=TRUE)
##---partmakesinb;
# Create the sin/cosine terms
plevel$cos <- cos(2*pi*plevel$YearMonth/1)
plevel$sin <- sin(2*pi*plevel$YearMonth/1)
head(plevel)
##---partmakesine;
sink()



sink('klamath-R-regfitsin.txt', split=TRUE)
##---partregfitsinb;
# Fit the regression line with the sin/cos term
# Because lm() produces type I (increment tests), you need to specify the
# year term last in the model.
# Be sure that month has been declared as a factor.
plevel.fit.sin <- lm( phos ~ sin + cos + Year , data=plevel)
drop1(plevel.fit.sin, test="F")
##---partregfitsine;
sink()


# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(plevel.fit.sin)
plotdiag

ggsave(plot=plotdiag, file='klamath-R-plotdiag.png')


##---partremoveoutb;
# Remove all outlier more than 0.20
plevel <- plevel[ plevel$phos < 0.20,]
##---partremoveoute;

sink('klamath-R-regfitsin2.txt', split=TRUE)
##---partregfitsin2b;
# Fit the regression line with the sin/cos term
# Because lm() produces type I (increment tests), you need to specify the
# year term last in the model.
# Be sure that month has been declared as a factor.
plevel.fit.sin2 <- lm( phos ~ sin + cos + YearMonth , data=plevel)
#drop1(plevel.fit.sin2, test="F")
summary(plevel.fit.sin2)
##---partregfitsin2e;
sink()



sink("klamath-R-fitpieces.txt", split=TRUE)
##---partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because month is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the month effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(plevel.fit.sin2)
coef(plevel.fit.sin2)
sqrt(diag(vcov(plevel.fit.sin2))) # gives the SE
confint(plevel.fit.sin2)
names(summary(plevel.fit.sin2))
summary(plevel.fit.sin2)$r.squared
summary(plevel.fit.sin2)$sigma
##---partfitpiecese;
sink()



##---partdiagplot2b;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag2 <- sf.autoplot.lm(plevel.fit.sin2)
plotdiag2
##---partdiagplot2e;

ggsave(plot=plotdiag2, file='klamath-R-diagplot2.png')

# Get the diagnostic plots using Base R graphics
layout(matrix(1:4,2,2))
plot(plevel.fit.sin2)
layout(1)


sink("klamath-R-dwtest2.txt", split=TRUE)
##---partdwtest2b;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(plevel.fit.sin2) # from the car package
  dwtest(plevel.fit.sin2) # from the lmtest package

##---partdwtest2e;
sink()



sink('klamath-R-predsetup.txt', split=TRUE)
##---partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(YearMonth=seq(min(plevel$Year,na.rm=TRUE),
                                 max(plevel$Year,na.rm=TRUE)+1,.1)) 
newYears$cos <- cos(2*pi*newYears$YearMonth/1)
newYears$sin <- sin(2*pi*newYears$YearMonth/1)
newYears[1:5,]
head(newYears)
##---partpredsetupe;
sink()

sink('klamath-R-predavg.txt', split=TRUE)
##---partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(plevel.fit.sin2, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)

plotfit <-  ggplot(data=plevel, aes(x=YearMonth, y=phos))+
  ggtitle("Phosphorus levels over time - outliers removed")+
  xlab("Year")+ylab("Phosphorus levels (mg/L)")+
  geom_point(size=4)+
  geom_line() + 
  geom_line(data=predict.avg.df, aes(x=YearMonth, y=fit), color="red")
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, 
                aes(x=YearMonth,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##---partpredavge;
sink()


ggsave(plot=plotfit, file='klamath-R-plotfit-sin2.png')
ggsave(plot=plotfit.avgci, file='klamath-R-plotpredavg-sin2.png')

# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to specify the y axis limits in advance of the plot
with(plevel, plot(YearMonth, phos,
    main='log(Phosphorus) over time',
    xlab='Year',ylab="log(Phosphorus) (mg/L)",
    ylim=range(c(plevel$phos, predict.avg.df$lwr  , predict.avg.df$upr), na.rm=TRUE))
    )  
  with(predict.avg.df, lines(YearMonth, fit))
  with(predict.avg.df, lines(YearMonth, lwr, lty=2))
  with(predict.avg.df, lines(YearMonth, upr, lty=2))



sink('klamath-R-predindiv.txt', split=TRUE)
##---partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(plevel.fit.sin2, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, 
                aes(x=YearMonth,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##---partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='klamath-R-plotpredindiv-sin2.png')


# Ditto in Base R graphics.
# Shading is hard to do unless you use the polygon function (give me a call)
# Notice how we need to reset the plotting limits IN ADVANCE
with(plevel, plot(YearMonth, phos,type="b",
    main='log(Phosphorus) over time',
    xlab='Year',ylab="log(Phosphorus) (mg/L)",
    ylim=range(c(plevel$phos, predict.avg.df$lwr  , predict.avg.df$upr,
                       predict.indiv.df$lwr, predict.indiv.df$upr), na.rm=TRUE))
    )
  with(predict.indiv.df, lines(YearMonth, fit))
  with(predict.indiv.df, lines(YearMonth, lwr, lty=2))
  with(predict.indiv.df, lines(YearMonth, upr, lty=2))
  with(predict.indiv.df, lines(YearMonth, lwr, lty=3, col="green"))
  with(predict.indiv.df, lines(YearMonth, upr, lty=3, col="green"))



####################################################################################
####################################################################################
# Seasonal affects with autocorrelation in residuals
# Normally the ar(1) structure would be fit to the residuals as seen in previous
# examples. However, in this example, there are many missing values so fitting a 
# simple ar(1) doesn't work well because if there are 3 months missing in the data,
# the ar(1) model would treat the 1st and 4th month (around the missing months) as correlated
# with a lag of 1 rather than a lag of 3.
#
# To get around these problems, use the spatial autocorrelation structures, where the
# "spatial" aspect is simply the temporal aspect. Here you need to define a variable
# for the "months" since the start of the experiment so that the structure can figure out
# the 'distance' between any two point given their month-from-start value.
#

plevel$cos <- NULL
plevel$sin <- NULL

sink("klamath-R-tempdist.txt", split=TRUE)
##---parttempdistb;
# Compute the "distance" from start of the experiment
plevel$MonthFromStart <- plevel$Year*12 + as.numeric(as.character(plevel$month)) -
                         (min(plevel$Year,na.rm=TRUE)*12 + 1)
plevel[1:15,]
##---parttempdiste;
sink()

# declare month as a categorical (factor) variable
plevel$month <- factor(plevel$month)
# remove any missing values
plevel <- plevel[!is.na(plevel$phos),]

# Fit the simple ANCOVA model. You could also use lm()
# but I wanted the same structure in both fits
base.fit <- gls(phos ~ month + Year, data=plevel)
summary(base.fit)

# Extract just the estimate of the common slope
sink("klamath-R-basefitest.txt",split=TRUE)
summary(base.fit)$tTable[ row.names(summary(base.fit)$tTable)=='Year',]
sink()

##---partCAR1b;
# Continuous AR(1) process with intial value of .5 for the correlation 1 month apart
car1.fit <- gls(phos~ month + Year , data=plevel,
                correlation=corCAR1(.5, ~MonthFromStart))
##---partCAR1e;

summary(car1.fit)

# Extract the autocorrelation value using R magic
# Dr. Google is your friend.
# http://r.789695.n4.nabble.com/Extracting-Phi-from-gls-lme-td803811.html
sink("klamath-R-CAR1phi.txt",split=TRUE)
temp <- car1.fit$modelStruct$corStruct 
coef(temp,unconstrained=FALSE)
cat("Est sigma is : ", summary(car1.fit)$sigma**2, "\n")
sink()

sink("klamath-R-CAR1est.txt", split=TRUE)
# Extract just the estimate of the common slope
summary(car1.fit)$tTable[ row.names(summary(car1.fit)$tTable)=='Year',]
sink()

sink('klamath-R-CAR1AIC.txt',split=TRUE)
# Do a model comparison
anova(base.fit, car1.fit)
sink()



###################################################################################
# Seasonal Kendall test for trend. Need to do this for EACH month.
# You cannot test for parallelism of trends in months using Kendall's tau
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# Read back in the data as we don't have to remove outiers
# Read in the data and restructure it into the long format
temp <- read.csv("klamath.csv", header=TRUE, as.is=TRUE,
                 strip.white=TRUE, na.string=".")
#temp
temp$X <- NULL
plevel <- melt(temp, id.var="month",
               measure.vars=names(temp)[grep('X',names(temp))],
               variable.name="XYear",
               value.name="phos")
plevel$Year      <- as.numeric(substr(as.character(plevel$XYear),2,5))
plevel$XYear     <- NULL
plevel$YearMonth <- plevel$Year + (plevel$month+.5)/12

# Sort the data by year and month
plevel <- plevel[order(plevel$Year, plevel$month),]

# Compute Kendall's tau for each month separately
month.kendall <- ddply(plevel,"month", function(x){
  # Apply kendalls method to each month
  #cat("\n\n***** Kendalls tau for month: ", as.character(x$month[1]),"\n")
  tau.test <- MannKendall(x$phos)
  #browser()
  #print(summary(tau.test))
  # Notice that the test are invariant to transformations
  tau.test.log <- MannKendall(x$phos)
  #print(tau.test.log)
  # compute the exact p0value
  tau.test3 <- cor.test( na.omit(x$phos),1:length(na.omit(x$phos)),
                        method= "kendall")
  res<- c(unlist(tau.test), tau.test3$p.value)
  names(res)[2] <- 'Approx.pvalue'
  names(res)[length(res)] <- 'Exact.pvalue'
  return(res)
})
month.kendall

# Convert the Exact p-value to a zscore
month.kendall$zscore <- abs(qnorm(month.kendall$Exact.pvalue/2))*sign(month.kendall$tau)

# Find the total z-score and a pvalue
totalz <- sum(month.kendall$zscore)
total.pvalue <- pnorm(-abs(totalz), sd=sqrt(12))*2
total.pvalue


# The Seasonal Mann Kendall test is available. The function assumes
# that the data are monthly values and that every month is present.
# Consequently, you should check this and pad with NAs as needed

# We notice that there are lots of missing values in the dataset
xtabs(~Year+month, data=plevel)

# We create a full dataset and then merge it with our dataset with missing values
full <- expand.grid(Year=seq(min(plevel$Year, na.rm=TRUE),
                             max(plevel$Year, na.rm=TRUE),1),
                    month=1:12)
plevel.full <- merge(plevel, full, all=TRUE )
xtabs(~Year+month, data=plevel.full)
xtabs(phos~Year+month, data=plevel.full, sparse=TRUE)

# order the data by year and month
plevel.full <- plevel.full[order(plevel.full$Year, plevel.full$month),]

# Do the SeaonalKendall after converting to a time-series object
plevel.ts <- ts(plevel.full$phos, frequency=12) 
plevel.ts
frequency(plevel.ts)

sink('klamath-R-seasonalMK.txt',split=TRUE)
##---partseasonalMKb;
SMK <- SeasonalMannKendall(plevel.ts)
summary(SMK)
##---partseasonalMKe;
sink()
