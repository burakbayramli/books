# Degradation of dioxin in crabs - II.
# 2015-07-21 CJS update for ggplot, sink, remove Base R, compare slopes, lsmeans; ##***
# 2014-05-04 CJS first editions

# How fast does diogin degrade over time? In each Year, samples of crabs
# were captured at a site. The crab livers were excised, composited #together, and various species of dioxings were measured. These were #translated into
# the World Health Organization (WHO) standardized total equivalent dose.

# This was done for two different sites. Is the degradation rate the same
# at both sites?

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
library(plyr)

#source("../../schwarz.functions.r")
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")




# Read in the data. Declare Site as a factor
sink('dioxin2-R-data.txt', split=TRUE)
##***partdatab;
crabs <- read.csv("dioxin2.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
crabs$Site   <- factor(crabs$Site)
crabs$logTEQ <- NULL # drop this and recompute later
head(crabs)
str(crabs)
##***partdatae;
sink()

# make an initial plot of the data
# Notice how we specify a different plotting symbol for each site.
##***partprelimplotb;
plotprelim <- ggplot(data=crabs, aes(x=Year, y=WHO.TEQ, 
                      shape=Site, color=Site))+
  ggtitle("Dioxin levels over time")+
  xlab("Year")+ylab("Dioxin levels (WHO.TEQ)")+
  geom_point(size=4)
plotprelim
##***partprelimplote;
ggsave(plot=plotprelim, file='dioxin2-R-prelimplot.png')


# Find the logTEQ and then get a revised plot

sink('dioxin2-R-002a.txt', split=TRUE)
##***part002ab;
crabs$logTEQ <- log(crabs$WHO.TEQ)
head(crabs)
##***part002ae;
sink()

# Repeat the plots on the log-scale
##***partprelimplotlogb;
plotprelimlog <- ggplot(data=crabs, aes(x=Year, y=logTEQ, shape=Site, color=Site))+
  ggtitle("log(Dioxin) levels over time")+
  xlab("Year")+ylab("log(Dioxin) levels (WHO.TEQ)")+
  geom_point(size=4)
plotprelimlog
##***partprelimplotloge;
ggsave(plot=plotprelimlog, file='dioxin2-R-prelimplotlog.png', h=4, w=6, units="in", dpi=300)


sink('dioxin2-R-regfitsep.txt', split=TRUE)
##***partregfitsepb;
# Fit a separate line for each year
d_ply(crabs, "Site", function(x){
  # fit a separate line for each site
  cat("\n\n***Separate fit for site :", as.character(x$Site[1]),"\n")
  fit <- lm( logTEQ ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
})
##***partregfitsepe;
sink()




sink('dioxin2-R-regfitnp.txt', split=TRUE)
##***partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table.
# Because lm() produces type I (increment tests), you should specify the
# contrast in the fit and use the Anova() function from the car package
crabs.fit.np <- lm( logTEQ ~ Site + Year + Year:Site, data=crabs,
                    contrast=list(Site='contr.sum'))
Anova(crabs.fit.np, type="III")
##***partregfitnpe;
sink()


sink('dioxin2-R-compslopes.txt', split=TRUE)
##***partcompslopeb;
# estimate the individual slopes and compare them 
crabs.fit.np.lsmo <- lsmeans::lstrends(crabs.fit.np, ~Site, var="Year")
summary(crabs.fit.np.lsmo, infer=TRUE)
summary(pairs(crabs.fit.np.lsmo), infer=TRUE)
##***partcompslopee;
sink()



sink('dioxin2-R-regfitp.txt', split=TRUE)
##***partregfitpb;
# Fit the regression line with parallel slopes. 
# Because lm() produces type I (increment tests), you should specify the
# contrast in the fit and use the Anova() function from the car package
crabs.fit.p <- lm( logTEQ ~ Year + Site, data=crabs,
                   contrast=list(Site='contr.sum'))
Anova(crabs.fit.p, type='III')
##***partregfitpe;
sink()


sink("dioxin2-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Site is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the site effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(crabs.fit.p)
coef(crabs.fit.p)
sqrt(diag(vcov(crabs.fit.p))) # gives the SE
confint(crabs.fit.p)
names(summary(crabs.fit.p))
summary(crabs.fit.p)$r.squared
summary(crabs.fit.p)$sigma
##***partfitpiecese;
sink()



sink("dioxin2-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test.
# You will need to do this for EACH site as the two sites are measured
# in the same year.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

d_ply(crabs, "Site", function(x){
  # apply the DW test to each sites regression lines
  cat("***** DW test for site :", as.character(x$Site[1]),"\n")
  crabs.fit <- lm(logTEQ ~ Year, data=x)
  print(durbinWatsonTest(crabs.fit)) # from the car package
  print(dwtest(crabs.fit)) # from the lmtest package
})
##***partdwteste;
sink()

sink('dioxin2-R-lsmeans.txt', split=TRUE)
##***partlsmeansb;
# Estimate the size of the site effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
crabs.fit.p.lsmo <- lsmeans::lsmeans(crabs.fit.p, ~Site)
sitediff <- pairs(crabs.fit.p.lsmo)
summary(sitediff, infer=TRUE)
##***partlsmeanse;
sink()



# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



sink('dioxin2-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(crabs$Year,na.rm=TRUE),2030,1), Site=unique(crabs$Site))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('dioxin2-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(crabs.fit.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)
temp <- predict.avg.df[predict.avg.df$Year==2010,]
temp
cat("Estimates of average amount of dioxin on the anti-log scale \n")
cbind(temp[,c("Year","Site")], exp(temp[,c("fit","lwr","upr")]))

plotfit <- plotprelimlog + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit))
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)+
    xlim(c(1990,2010))
plotfit.avgci
##***partpredavge;
sink()
ggsave(plot=plotfit, file='dioxin2-R-plotfit.png', h=4, w=6, units="in", dpi=300)
ggsave(plot=plotfit.avgci, file='dioxin2-R-plotpredavg.png', h=4, w=6, units="in", dpi=300)


# You can plot everything back on the anti-logscale as well - try it!


sink('dioxin2-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(crabs.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
predict.indiv.df    [predict.indiv.df$Year==2010,]
temp <- predict.avg.df[predict.avg.df$Year==2010,]
temp
cat("Estimates of individual levels of dioxin on the anti-log scale \n")
cbind(temp[,c("Year","Site")], exp(temp[,c("fit","lwr","upr")]))


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='dioxin2-R-plotpredindiv.png', h=4, w=6, units="in", dpi=300)



##***partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(crabs.fit.p)
plotdiag
##***partdiagplote;
ggsave(plot=plotdiag, file='dioxin2-R-diagplot.png', h=6, w=6, units="in", dpi=300)



# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended

##***partinvpredb;
plotinvpred <- plotfit.indivci +
  geom_hline(yintercept=log(10))+
  xlim(c(1990, 2030))
plotinvpred
##***partinvprede;
ggsave(plot=plotinvpred, file='dioxin2-R-plotinvpred.png', h=4, w=6, units="in", dpi=300)




###################################################################################
# Kendall test for trend. Need to do this for EACH site.
# You cannot test for parallelism of trends in sites using Kendall's tau
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

d_ply(crabs,"Site", function(x){
  # Apply kendalls method to each site
  cat("\n\n***** Kendalls tau for site: ", as.character(x$Site[1]),"\n")
  tau.test <- MannKendall(x$WHO.TEQ)
  print(summary(tau.test))
  # Notice that the test are invariant to transformations
  tau.test.log <- MannKendall(x$logTEQ)
  print(tau.test.log)
})


# Now to estimate the slope (on the log-scale) using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(logTEQ~Year, data=crabs)
sen.slope$coef
confint.zyp(sen.slope)





































