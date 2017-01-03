# Yukon River Breakup
# 2014-11-21 CJS remove base R graphics; added break point; fix ##--- problem
# 2014-05-03 CJS First edition

# As taken from CBC News: 
#   http://www.cbc.ca/news/canada/north/winner-announced-for-yukon-river-break-up-prediction-1.2630676
# The ice is out on the Yukon River at Dawson City — and the winner has been announced 
# for the annual contest where people predict the timing of the breakup. 

# The tripod in the annual "ice pool" moved at 1:19 p.m. Friday, 2014-05-02. 
# That's almost two weeks earlier than last Year when the official break-up occurred on May 15.

# But that didn't throw off Dawson City resident Austin Gavin, 
# who has the winning ticket for this Year's competition. 
# She's now more than $4,000 richer after guessing the break-up time correctly.

# The Imperial Order Daughters of the Empire (IODE) women's 
# charity splits the proceeds of the pool 50/50 with the winner.

# Dawne Mitchell, also from Dawson City, pocketed just over 
# $3,800 for her correct prediction last Year.

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

# See
#  http://www.yukonriverbreakup.com
# for more details

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
sink('yukon-R-010.txt', split=TRUE)
##***part010b;
ice <- read.csv("yukon.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string="NA")
head(ice)
str(ice)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=ice, aes(x=Year, y=Jdate))+
  ggtitle("Yukon River Breakup")+
  xlab("Year")+ylab("Breakup (Julian Date)")+
  geom_point()+
  geom_line()
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='yukon-R-prelimplot.png')


# Fit the regression line and get the results
sink('yukon-R-regfit.txt', split=TRUE)
##***partregfitb;
ice.fit <- lm( Jdate ~ Year, data=ice)
summary(ice.fit)
##***partregfite;
sink()

sink("yukon-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(ice.fit)
coef(ice.fit)
sqrt(diag(vcov(ice.fit))) # gives the SE
confint(ice.fit)
names(summary(ice.fit))
summary(ice.fit)$r.squared
summary(ice.fit)$sigma
##***partfitpiecese;
sink()

##***partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(ice.fit)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='yukon-R-diagplot.png')


sink("yukon-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

durbinWatsonTest(ice.fit) # from the car package
dwtest(ice.fit) # from the lmtest package
##***partdwteste;
sink()

##***partplotfitb;
# plot the fitted line to the graphs
plotfit <- plotprelim + 
     geom_abline(intercept=coef(ice.fit)[1], slope=coef(ice.fit)[2])
plotfit
##***partplotfite;

ggsave(plot=plotfit, file='yukon-R-plotfit.png')

# or we can use the builtin smoothing functions of ggplot which also gives 
# a confidence interval for the MEAN response
plotfit2 <- plotprelim +
     geom_smooth(method="lm")
plotfit2


sink('yukon-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- data.frame(Year=seq(min(ice$Year,na.rm=TRUE),3+max(ice$Year,na.rm=TRUE),1))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('yukon-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE time of breakup for each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(ice.fit, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()

ggsave(plot=plotfit.avgci, file='yukon-R-plotpredavg.png')


sink('yukon-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL time of breakup at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(ice.fit, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='yukon-R-plotpredindiv.png')


# No easy way to do inverse predictions


###################################################################################
# Cox and Stuart Test for trend
# This is simply a sign.test applied to the paired data
# It is available in the randtests package
# You must make sure that the data is sorted by time ordering and that all missing
# values are removed

sink("yukon-R-coxstuart.txt",split=TRUE)
##***partcoxstuartb;
ice <- ice[order(ice$Year),]
Jdate.no.missing <- ice$Jdate[!is.na(ice$Jdate)]
cox.stuart.test(Jdate.no.missing)
##***partcoxstuarte;
sink()


##############################################################################
# Spearman's rho (non-parametric correlation)

sink('yukon-R-spearman.txt',split=TRUE)
##***partspearmanb;
# Because there is no distiction between the response and predictor variables,
# the formula is specified with both variables to the right
# of the tilde
cor.test( ~Jdate + Year, data=ice, method="spearman")
##***partspearmane;
sink()

###################################################################################
# Kendall test for trend
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# The MannKendall test is found in the Kendall package
# or it can be computed using the cor.test() function as well

sink('yukon-R-kendalltau.txt',split=TRUE)
##***partkendalltaub;
tau.test <- Kendall(ice$Jdate, ice$Year)
summary(tau.test)
tau.test2 <-cor.test( ~Jdate + Year, data=ice, method="kendall")
tau.test2
##***partkendalltaue;
sink()

sink('yukon-R-senslope.txt',split=TRUE)
##***partsenslopeb;
# Now to estimate the slope using the Sen estimator in the zyp package
sen.slope <- zyp.sen(Jdate~Year, data=ice)
sen.slope$coef
confint.zyp(sen.slope)
##***partsenslopee;
sink()


#####################################################################
# Is there a breakpoint in the data
# Use the SiZer package

library(SiZer)

# The piecewise.linear function is not very forgiving of missing values.
# Remove all of these before the fit.
dim(ice)
ice.nm <- ice[ !is.na(ice$Jdate),]
dim(ice.nm)

sink('yukon-R-breakpoint.txt', split=TRUE)
##***part-breakpointb;
# Fit the a broken stick to the Julian Date using the SiZer package
pw.model <- piecewise.linear(ice.nm$Year, ice.nm$Jdate, middle=1, CI=TRUE,
                             bootstrap.samples = 100, sig.level = 0.05)
pw.model
##***part-breakpointe;
sink()

# Make some predictions from the breakpoint model
pw.model.res <- data.frame(Year=seq(min(ice$Year,na.rm=TRUE),max(ice$Year,na.rm=TRUE)))
pw.model.res$mean.bp <- predict(pw.model, x=pw.model.res$Year)
head(pw.model.res)
  
# Plot the unknown breakpoint model
plot3 <- plotfit+
   geom_line(data=pw.model.res, aes(x=Year, y=mean.bp), color="green", size=2,linetype=1)
plot3

ggsave(plot=plot3, file='yukon-R-breakpoint-plot.png', dpi=300,
       height=4, width=6, units="in")



