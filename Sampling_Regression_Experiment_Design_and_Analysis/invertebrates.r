# Comparing regressions line with pseudo-replication

# 2014-08-14 CJS First edition

# Weins and Parker (1985) discuss a variety of design suitable for monitoring environmental 
# impact when little (or no) pre-project data are available. 
# Their ``impact-level-by-time interaction'' design is very similar to the proposal --
# monitor the project and control site to see if the response over time follows a parallel trajectory. 
 
# This example is  (loosely) patterned on monitoring the density of aquatic invertebrates over time
# in the absence of pre-impact data. Two streams were monitored running through a recent clearcut.
# As the clearcut gradually revegetates, the community ecology of the stream changes and so a decline
# in a particular invertebrate over time is envisioned. Around one of the streams, several enhancements
# to speed the regeneration of the forest are tried. Is there evidence that these enhancements change
# the rate at which the invertebrate population changes?
 
# In each of seven years of monitoring, five samples of invertebrates were sampled at 
# each stream. 

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(lmerTest)
library(lsmeans)
library(plyr)
library(reshape2)

source("../../schwarz.functions.r")

sink('invertebrates-R-readdata.txt', split=TRUE)
##***part-readdatab;
invert <- read.csv('invertebrates.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
invert$Site <- factor(invert$Site)
invert[1:10,]
##***part-readdatae;
sink()


#------------------------ Naive Fit ------------------------------
# Fit the naive regression line
fit.naive <- lm( Density ~ Year, data=invert)

# plot the naive fit on the data point
plot.naive <- ggplot(data=invert, 
    aes(x=Year, y=Density, shape=Site, group=Site))+
    ggtitle("Density vs Year with naive fit")+
    xlab("Year)\nPoints jittered")+ylab("Density")+
    geom_point(size=3, position=position_jitter(width=0.05))+
    geom_smooth(method='lm', se=FALSE )
plot.naive
ggsave(plot=plot.naive, file='invertebrates-R-prelimplot.png', height=4, width=6, units="in")






#---------------------------- Fit on the Average -----------------
sink("invertebrates-R-findavg.txt", split=TRUE)
##***part-findavgb;
invert.avg <- ddply(invert, c("Site","Year"), function(x){
    # compute the averages over the Sites
    Density.avg<- mean(x$Density)
    names(Density.avg) <- "Density.avg"
    return(Density.avg)
})
invert.avg
##***part-findavge;
sink()

sink("invertebrates-R-diffs.txt", split=TRUE)
##***part-dcastb;
# Reshape the data so that you get both readings on the same line
invert.avg.paired <- dcast(invert.avg, Year ~ Site)
invert.avg.paired$diff <- invert.avg.paired$"Control Site" - invert.avg.paired$"Project Site"
invert.avg.paired
##***part-dcaste;
sink()


# Do a regression on the differences
diff.fit <- lm( diff ~ Year, data=invert.avg.paired)
sink("invertebrates-R-diffsreg.txt", split=TRUE)
summary(diff.fit)$coefficients
sink()

diff.plot <- ggplot(data=invert.avg.paired, aes(x=Year, y=diff))+
   ggtitle("Paired differences over time")+
   xlab("Year")+ylab("Difference in site averages")+
   geom_point(size=3)+
   geom_smooth(method="lm", se=FALSE)
diff.plot
ggsave(plot=diff.plot, file='invertebrates-R-diffplot.png', height=4, width=6, units="in")


sink("invertebrates-R-kendall.txt", split=TRUE)
##***part-kendallb;
# Compute Kendall's tau
cor.test(invert.avg.paired$diff, invert.avg.paired$Year, method="kendall") 
##***part-kendalle;
sink()



#---------------------------- ANCOVA on the averaged values ------------
##***part-ancovaavg-yearcopyb;
invert.avg$YearF <- as.factor(invert.avg$Year)
##***part-ancovaavg-yearcopye;

##***part-ancovaavgb;
fit.mixed <- lmerTest::lmer( Density.avg ~ Year + Site + Year:Site +
                            (1|YearF), data=invert.avg)
##***part-ancovaavge;


sink("invertebrates-R-ancovaavg-anova.txt", split=TRUE)
anova(fit.mixed, ddf="Kenward-Roger")
sink()

sink("invertebrates-R-ancovaavg-varcomp.txt", split=TRUE)
VarCorr(fit.mixed)
sink()

# Estimate the individual slopes and their difference
sink("invertebrates-R-ancovaavg-estslopes.txt", split=TRUE)
##***part-ancovaavg-estslopesb;
slope.lsmo <- lsmeans::lstrends(fit.mixed, ~Site, var="Year")
summary(slope.lsmo)
pairs(slope.lsmo)
##***part-ancovaavg-estslopese;
sink()

# Estimate the individual slopes
fixedeff <- fixef(fit.mixed)
vcov     <- vcov (fit.mixed)

e <- matrix(c(0,1,0,0, 0,1,0,1), nrow=2, byrow=TRUE)
e
slopes <- e %*% fixedeff
slopes.se <- sqrt(diag(e %*% vcov %*% t(e)))
cbind(slopes, slopes.se)
##***part-ancovaavge;



#---------------------------- Fit the individual values using a mixed model -----------------

##***part-yearcopy2b;
invert$YearF <- factor(invert$Year)
##***part-yearcopy2e;

##***part-fitmixedb;
fit.mixed.indiv <- lmerTest::lmer( Density ~ Year + Site + Year:Site +
                                     (1|YearF) + (1|YearF:Site), data=invert)
##***part-fitmixede;

sink("invertebrates-R-regfit-mixed-anova.txt", split=TRUE)
anova  (fit.mixed.indiv, ddf="Kenward-Roger")
sink()

# Estimate the difference in slopes
# This is the interaction term when the default contrast matrix is used and there
# are only two groups.
summary(fit.mixed.indiv)
sink("invertebrates-R-regfit-mixed-summary.txt", split=TRUE)
##***part-regfit-mixed-indivd-estslopesb;
slope.lsmo <- lsmeans::lstrends(fit.mixed.indiv, ~Site, var="Year")
summary(slope.lsmo)
pairs(slope.lsmo)
##***part-regfit-mixed-indiv--estslopese;
sink()

# get the variance components
sink("invertebrates-R-regfit-mixed-varcomp.txt", split=TRUE)
##***part-fitmixed-varcorrb;
VarCorr(fit.mixed.indiv)
##***part-fitmixed-varcorre;
sink()


