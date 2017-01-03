# Hibernation energetics.
# 2015-10-10 - fix contrast in lm()
# 2014-02-04 - Convert to ggplot; lsmeans() only for multiple comparisions
# 2014-07-20 - Fixup for new version of Rstudio; simultaneous intervals in lsmeans

# The energy consumption of mice  at different temperatures of 
# hibernation and different amount of food were recorded.

# Example of a two factor CRD analysis of variance with unbalanced data

# Lines with ##***partxxb; and ##***partxxe; are used to select code for my notes and can be ignored.

cat(" Effect of temp and diet on energy levels in Fish ", date(),"\n\n")

options(useFancyQuotes=FALSE) # renders summary output corrects
options(width=200)

library(car)
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r') # load the helper functions


# Read in the actual data
sink("mouse-R-001.txt", split=TRUE)
##***part001b;
energy <- read.csv("mouse.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
energy$trt <- interaction(energy$temp, energy$diet)
# Convert to factors
energy$tempF <- factor(energy$temp)
energy$dietF <- factor(energy$diet)
energy$trtF  <- factor(energy$trt)
cat("Listing of part of the raw data \n")
energy[1:10,]
##***part001e;
sink()


str(energy)

# Preliminary plots

# Get side-by-side dot plots
##***part010b;
plot010 <- ggplot(energy, aes(x=trt, y=energy))+
     geom_jitter(size=3, position=position_jitter(width=0.2, height=0.1))+
     geom_boxplot(alpha=0.2, notch=TRUE)+
     expand_limits(y=170)+  # get all of box plot for second group
     xlab("Energy expenditure\n Point jittered to prevent overplotting")+
     ylab("Energy expenditure")+
     ggtitle("Energy expenditure of pocket mice with overlaid boxplots")
plot010
##***part010e;

ggsave(plot010, file="mouse-R-010.png", height=4, width=6, units="in", dpi=300) 


sink('mouse-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(energy, c("tempF","dietF"), sf.simple.summary, variable="energy", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part020e;
sink()


# Get some simple summary statistics doing the computations explicitly
report2 <- ddply(energy, c("tempF","dietF"), function(x){
  # Compute summary stats on this subset of the data
  
  mean <- mean  (x$energy, na.rm=TRUE)
  sd   <- sd    (x$energy, na.rm=TRUE)
  n    <- length(x$energy)
  nmiss<- n-length(na.omit(x$energy))
  se.mean<-sd / sqrt(n-nmiss) # because a crd (avoid missing values)
  ci   <- t.test(x$energy)$conf.int
  res  <- c(n,nmiss, mean, sd, se.mean, ci)
  names(res) <- c("n", "nmiss", "mean", "sd", "se.mean","lcl.mean","ucl.mean")
  res
})
cat("\n\n Summary report \n")
report2


str(report)


# Check to see if the variance is increasing with the mean
plot011 <- ggplot(report, aes(x=log(mean), y=log(sd)))+
      ggtitle("See if the SD increases with the mean")+
      xlab("log(Mean energy expenditure)")+
      ylab("log(SD energy expenditure)")+
      geom_point(size=3)
plot011

# Yes the SD seems to increase with the mean
# so an analysis of log(energy) analysis may be a better choice.
# But we continue with the analysis on the regular scale


# Draw a profile plot based on the summary data generated
##***part030b;
plot030 <- ggplot(report, aes(x=dietF, y=mean, group=tempF, color=tempF, linetype=tempF))+
     ggtitle("Interaction plot based on raw data")+
     xlab("Diet \n Jittering applied to prevent overplotting")+ylab("Mean energy expenditure with 95% ci")+
     geom_pointrange(aes(ymin=lcl, ymax=ucl),position = position_dodge(0.05),size=1)+
     geom_line(size=2)
plot030
##***part030e;

ggsave(plot030, file="mouse-R-030.png", height=4, width=6, units="in")  # send the plot to a png file



# Fit the linear model, get the anova table, and the usual stuff
# CAUTION!!! Because the design is unbalance, the default model
# fit by aov() and lm() gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html
#
# You can also use the Anova() function from the car package.
sink('mouse-R-100.txt', split=TRUE)
##***part100b;
cat("The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
result.lm <- lm( energy ~ tempF + dietF + tempF:dietF, data=energy)
cat("\n\n Analysis of variance -- this is NOT CORRECT because design is unbalanced \n")
anova(result.lm)


cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat(  "\nBEFORE fitting the lm() model!")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")
result.lm2 <- lm( energy ~ tempF + dietF + tempF:dietF, data=energy,
                  contrasts=list(tempF="contr.sum", dietF="contr.sum"))
Anova(result.lm2,type=3)
##***part100e;
sink()


# Check the residuals etc using the schwarz autoplot functions
##***part100diagnosticb;
plotdiag <-sf.autoplot.lm(result.lm2, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***part100diagnostice;

ggsave(plot=plotdiag, file='mouse-R-100-diagnostic.png', height=6, width=6, units="in")




# LSmeans after a lm() fit
# A Tukey adjustment really isn't needed because there are only 2 levels
# The new lsmeans() package returns an object for which there are many interesting methods
sink('mouse-R-100LSM-temp.txt',split=TRUE)
##***part100LSM-tempb;
cat("\n\n Estimated marginal means for temperature effect \n\n")
lsmeans.temp <- lsmeans(result.lm2, ~tempF, adjust='tukey')
cld(lsmeans.temp) # individual marginal means and cld display
##***part100LSM-tempe;
sink()

# make a plot of the means and cld's
temp.cld <- cld(lsmeans.temp)
 
plot100LSM.temp <- sf.cld.plot.bar(temp.cld, variable="tempF")
plot100LSM.temp <- plot100LSM.temp + 
        xlab("Temperature")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure by temperature with cld")
plot100LSM.temp

# Make a line graph of the cld display
plot101LSM.temp <- sf.cld.plot.line(temp.cld, variable="tempF")
plot101LSM.temp <- plot101LSM.temp+ 
        xlab("Temperature")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure by temperature with cld")
plot101LSM.temp



sink('mouse-R-100LSM-diet.txt', split=TRUE)
##***part100LSM-dietb;
cat("\n\n Estimated marginal means for diet \n\n")
lsmeans.diet <- lsmeans(result.lm2, ~dietF, adjust='tukey' )
diet.cld <- cld(lsmeans.diet)
diet.cld
##***part100LSM-diete;
sink()

# Similar plots could be made for diet as was done for  temperature
 
plot100LSM.diet <- sf.cld.plot.bar(diet.cld, variable="dietF")
plot100LSM.diet <- plot100LSM.diet + 
        xlab("Diet")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure by diet with cld")
plot100LSM.diet

# Make a line graph of the cld display
plot101LSM.diet <- sf.cld.plot.line(diet.cld, variable="dietF")
plot101LSM.diet <- plot101LSM.diet+ 
        xlab("Diet")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure by diet with cld")
plot101LSM.diet

# How about for the interaction term

sink('mouse-R-100LSM-int.txt', split=TRUE)
##***part100LSM-intb;
cat("\n\n Estimated marginal means for temp and diet combinations \n\n")
lsmeans.dt <- lsmeans(result.lm2, ~dietF:tempF, adjust="tukey")
cld(lsmeans.dt)
cat("Odd results because of very different sample sizes !\n")
##***part100LSM-inte;
sink()


# The confidence intervals for the individual means are not simultaneous. If we want
# simultaneous confidence intervals for each mean as well we can get them using
summary(lsmeans.dt,  adjust='tukey')


# Similar plots could be made as for temperature
# but notice how we plot the combination of the two factors on the x axis
dt.cld <- cld(lsmeans.dt)
# We want to sort the plot from largest to smallest
# so we need to define the interaction factor as an ordered factor
# based on the ordering in the cld table which is already ordered
dt.cld$dtF <- with(dt.cld, interaction(dietF, tempF, sep="\n"))
dt.cld$dtF <- factor(as.character(dt.cld$dtF), levels=dt.cld$dtF, order=TRUE)

plot100LSM.dt <- sf.cld.plot.bar(dt.cld, variable="dtF")
plot100LSM.dt <- plot100LSM.dt + 
        xlab("Temperature and Diet")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure with cld")
plot100LSM.dt

# Make a line graph of the cld display
plot101LSM.dt <- sf.cld.plot.line(dt.cld, variable="dtF")
plot101LSM.dt <- plot101LSM.dt+ 
        xlab("Temperature")+
        ylab("Mean energy expenditure with 95% ci")+
        ggtitle("Comparison of mean energy expenditure with cld")
plot101LSM.dt


# We can also plot the interaction plot (but no SE are plotted)
lsmip(lsmeans.dt, dietF ~ tempF)



# Get and display the pairwise  comparisons.
sink('mouse-R-100multcomp-temp.txt',split=TRUE)
##***part100multcomp-tempb;
cat("\n\n Estimated temperature effect\n")
temp.pairs <- pairs(lsmeans.temp)
summary(temp.pairs, infer=TRUE)
##***part100multcomp-tempe;
sink()


sink('mouse-R-100multcomp-diet.txt',split=TRUE)
##***part100multcomp-dietb;
cat("\n\n Estimated temperature effect\n")
diet.pairs <- pairs(lsmeans.diet)
summary(diet.pairs, infer=TRUE)
##***part100multcomp-diete;
sink()



# We can do something similar with the interaction terms and get tukey adjusted confidence intervals
dt.pairs <- pairs(lsmeans.dt)
dt.pairs
dt.pairs.ci <- confint(dt.pairs)
dt.pairs.ci

ggplot(data=dt.pairs.ci, aes(x=estimate, y=contrast))+
  ggtitle("Simultaneous confidence intervals for pairwise differences of all treatments")+
  xlab("Estimate and 95% ci")+ylab("Treatment")+
  geom_point(size=4)+
  geom_errorbarh(aes(xmin=lower.CL, xmax=upper.CL), height=0.1)+
  geom_vline(xintercept=0, linetype=2)











