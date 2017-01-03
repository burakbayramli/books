# Effect of age, weight, and stress index on blood pressure
# Multiple regresssion
#  2015-07-19 CJS First edition
 
# Blood pressure tends to increase with age, body mass, and potentially stress.
#   To investigate the relationship of blood pressure to these variables, a
#   sample of men in a large corporation was selected. For each subject,
#   their age (years), body mass (kg), and a stress index (ranges from 0 to 100)
#   was recorded along with their blood pressure.  
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#
options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the data
sink('bloodpress-R-data.txt', split=TRUE)
##***partdatab;
bp <- read.csv('bloodpress.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
head(bp)
##***partdatae;
sink()

##***partscatterb;
# Preliminary scatter plot matrix
scatter <- ggpairs(data=bp, columns=c(1,3,4,2), title='Preliminary scatter plot')
scatter
##***partscattere;

# unfortunately, scatter is not a ggplot2 so can't use ggsave (groan)
png('bloodpress-R-scatter.png', h=6, w=6, units="in", res=300)
scatter
dev.off()
scatter

# multiple regression
sink("bloodpress-R-lmanova.txt", split=TRUE)
##***partlmanovab;
# caution. the lm() function gives type I (increment) tests by default. YOu want type III (marginal)
bp.fit <- lm(BloodPressure ~ Age+StressIndex + Weight, data=bp)
cat("*** CAUTION *** These are incremental tests and not useful\n")
anova(bp.fit)

bp.fit <- lm(BloodPressure ~ Age+StressIndex + Weight, data=bp)
cat("*** These are the marginal tests and are useful\n")
Anova(bp.fit, type='III')
##***partlmanovae;
sink()

sink("bloodpress-R-lmnobs.txt", split=TRUE)
##***partlmnobsb;
# Number of observations used in the fit
nobs(bp.fit)
nrow(bp)
##***partlmnobse;
sink()

sink('bloodpress-R-lmoverallF.txt', split=TRUE)
##***partlmoverallFb;
# Extract the overall F-test and pvalue
# Refer to http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
c(summary(bp.fit)$fstatistic, 
    pvalue=pf(summary(bp.fit)$fstatistic[1],
              summary(bp.fit)$fstatistic[2],
              summary(bp.fit)$fstatistic[3],lower.tail=FALSE))
##***partlmoverallFe;
sink()


sink('bloodpress-R-lmsummary.txt', split=TRUE)
##***partlmsummaryb;
# Get the coeffient table
summary(bp.fit)
##***partlmsummarye;
sink()

sink("bloodpress-R-lmcoefci.txt", split=TRUE)
##***partlmcoefcib;
# Confidence intervals for each coefficient
confint(bp.fit)
##***partlmcoefcie;
sink()


sink("bloodpress-R-lmpredavg.txt", split=TRUE)
##***partlmpredavgb;
# get predictions of the mean response. THis ia real pain because of the missing values in the original data.
# So always specify the newdata=original data to get predictions for each row
# We need to save the predictions for the diagnostic plot of obs vs predicted below
bp.fit.predavg <- predict(bp.fit, newdata=bp, se.fit=TRUE, interval="confidence")
bp.fit2 <- cbind(bp, bp.fit.predavg$fit, se=bp.fit.predavg$se)
bp.fit2
##***partlmpredavge;
sink()

sink("bloodpress-R-lmpredindiv.txt", split=TRUE)
##***partlmpredindivb;
# get predictions of the individual responses.
bp.fit.predindiv <- predict(bp.fit, newdata=bp, se.fit=TRUE, interval="prediction")
cbind(bp, bp.fit.predindiv$fit)
##***partlmpredindive;
sink()


# diagnostic plots

##***partobsvspredb;
# plot the observed data vs. the predicted data
obsvspred <- ggplot(data=bp.fit2, aes(x=fit, y=BloodPressure))+
  ggtitle("Observed vs predicted")+
  xlab("Predicted value")+ylab("Observed value")+
  geom_point()+
  geom_abline(intercept=0, slope=1)
obsvspred
##***partobsvsprede;
ggsave(plot=obsvspred, file='bloodpress-R-obsvspred.png', h=4, w=6, units="in", dpi=300)


# The 4 panel plot
# get the residual plot and normal probability plot
# Sigh... in 2015-07, the authors of the gridExtra changed the functionality so now the revised code
# needs to be used.

png('bloodpress-R-diagplot.png', h=6, w=6, units="in", res=300)
##***partlmdiagplotb;
# diagnostic plots
diagplot <- sf.autoplot.lm(bp.fit)
grid.newpage()
grid.draw(diagplot)
##***partlmdiagplote;
dev.off()

grid.draw(diagplot)

png("bloodpress-R-lmleverage.png", h=6, w=6, units="in", res=300)
##***partlmleverageb;
# Leverage plots
leveragePlots(bp.fit)
##***partlmleveragee;
dev.off()
leveragePlots(bp.fit)



