# Fertilizer and growth of tomato plants
# 2015-04-17 CJS Misc schanges
# 2014-05-07 CJS updated with ggplot etc

# When I was living in Manitoba, I used to grow 
# tomato plants. One day my daughter saw me planting and asked 
# why I was putting fertilizer with the plants.
#  When I responded that it helped the plants grow better, 
# she asked #me if adding more fertilizer made the plants grow better.

# A learning opportunity presented itself.

# We randomly applied different amounts of fertilizer to different plots.
# Over the summer, we carefully watered all plots equally and weighed
# the amount of tomatoes produced.


# Lines starting in ##--part001b; and ##***part001e; are used
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

#source('../../schwarz.functions.r') # in case internet not availabe
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')


# Read in the data
sink('fertilizer-R-010.txt', split=TRUE)
##***part010b;
plants <- read.csv("fertilizer.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
head(plants)
str(plants)
##***part010e;
sink()


# make an initial plot of the data
##***partprelimplotb;
plotprelim <- ggplot(data=plants, aes(x=fertilizer, y=yield))+
  ggtitle("Yield vs. Fertilizer")+
  xlab("Fertilizer")+ylab("Yield")+
  geom_point(size=4)
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file='fertilizer-R-prelimplot.png', h=4, w=6, units="in",dpi=300)


# Fit the regression line and get the results
sink('fertilizer-R-regfit.txt', split=TRUE)
##***partregfitb;
plants.fit <- lm( yield ~ fertilizer, data=plants)
anova(plants.fit)
summary(plants.fit)
##***partregfite;
sink()


sink("fertilizer-R-confintslope.txt", split=TRUE)
##***partconfintslopeb;
confint(plants.lm)
##***partconfintslopee;
sink()

sink("fertilizer-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods
anova(plants.fit)
coef(plants.fit)
sqrt(diag(vcov(plants.fit))) # gives the SE
confint(plants.fit)
names(summary(plants.fit))
summary(plants.fit)$r.squared
summary(plants.fit)$sigma
##***partfitpiecese;
sink()

##***partdiagplotb;
# look at diagnostic plot
plotdiag <- sf.autoplot.lm(plants.fit)
plotdiag
##***partdiagplote;

ggsave(plot=plotdiag, file='fertilizer-R-diagplot.png', h=6, w=6, units="in",dpi=300)



##***partplotfitb;
# plot the fitted line to the graphs
plotfit <- plotprelim + 
     geom_abline(intercept=coef(plants.fit)[1], slope=coef(plants.fit)[2])
plotfit
##***partplotfite;

ggsave(plot=plotfit, file='fertilizer-R-plotfit.png', h=4, w=6, units="in",dpi=300)

# or we can use the builtin smoothing functions of ggplot which also gives 
# a confidence interval for the MEAN response
plotfit2 <- plotprelim +
     geom_smooth(method="lm")
plotfit2


sink('fertilizer-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newfertilizers <- data.frame(fertilizer=seq(min(plants$fertilizer,na.rm=TRUE),
                                            max(plants$fertilizer,na.rm=TRUE),1))
newfertilizers[1:5,]
str(newfertilizers)
##***partpredsetupe;
sink()

sink('fertilizer-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE yield at each fertilizer
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(plants.fit, newdata=newfertilizers, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newfertilizers, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=fertilizer,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()

ggsave(plot=plotfit.avgci, file='fertilizer-R-plotpredavg.png', h=4, w=6, units="in",dpi=300)



sink('fertilizer-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL yield at each fertilizer
# R does not product the se for individual predictions
predict.indiv <- predict(plants.fit, newdata=newfertilizers, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newfertilizers, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=fertilizer,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='fertilizer-R-plotpredindiv.png', h=4, w=6, units="in",dpi=300)


# No easy way to do inverse predictions


###################################################################################
# Cox and Stuart Test for trend
# This is simply a sign.test applied to the paired data
# It is available in the randtests package
# You must make sure that the data is sorted by time ordering and that all missing
# values are removed

sink("fertilizer-R-coxstuart.txt",split=TRUE)
##***partcoxstuartb;
plants <- plants[order(plants$fertilizer),]
yield.no.missing <- plants$yield[!is.na(plants$yield)]
cox.stuart.test(yield.no.missing)
##***partcoxstuarte;
sink()


##############################################################################
# Spearman's rho (non-parametric correlation)

sink('fertilizer-R-spearman.txt',split=TRUE)
##***partspearmanb;
# Because there is no distiction between the response and predictor variables,
# the formula is specified with both variables to the right
# of the tilde
cor.test( ~yield + fertilizer, data=plants, method="spearman")
##***partspearmane;
sink()

###################################################################################
# Kendall test for trend
# Need to order the data by amount of fertilizer (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

# The MannKendall test is found in the Kendall package
# or it can be computed using the cor.test() function as well

sink('fertilizer-R-kendalltau.txt',split=TRUE)
##***partkendalltaub;
tau.test <- Kendall(plants$yield, plants$fertilizer)
summary(tau.test)
tau.test2 <-cor.test( ~yield + fertilizer, data=plants, method="kendall")
tau.test2
##***partkendalltaue;
sink()

sink('fertilizer-R-senslope.txt',split=TRUE)
##***partsenslopeb;
# Now to estimate the slope using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(yield~fertilizer, data=plants)
sen.slope$coef
confint.zyp(sen.slope)
##***partsenslopee;
sink()


