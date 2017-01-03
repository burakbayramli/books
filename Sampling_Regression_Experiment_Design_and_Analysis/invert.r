# Invertebrate data from a simple before/after analysis.
# 2014-11-28 CJS sf.autoplot.lmer() added
# 2014-11-21 CJS fixes to sink/split; use ggplot rather than Base graphics;  fix read.csv; use lsmeans to estimate period effects
#                fix ##--- problem
# 2012-08-27 CJS First edition

#   Two streams were measured at a small hydro electric power project. At each stream
#   multiple samples were taken on the steam, and the number of invertebrates
#   was counted.

#   After 5 years, the hydro electric plant started up, and an additional 5 years
#   of data were collected.
 
#   Is there evidence of a change in abundance after the plant starts?

#  Note that this is NOT a BACI design, and is a VERY poor substitute for such.
#  The key problem is that changes may have occured over time unrelated
#   to the impact, so a "change" may simple be natural. 

#  Also, in this analysis we have ignored autocorrelation which can be
#   a serious problem in long-term studies. 

# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects


library(ggplot2)
library(lattice)
library(lmerTest)
library(lsmeans)
library(plyr)

source("../../schwarz.functions.r")

# Read in the data; declare Period as a factor; create factor for Year
sink('invert-R-001.txt', split=TRUE)
##***part001b;
invert <- read.csv('invert.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
# Declare Period as an ordered factor so that sorts in proper order
invert$Period  <- factor(invert$Period, levels=c("Before","After"), ordered=TRUE)
# We also want Year to be categorical variable so make a new variable
invert$YearF   <- factor(invert$Year)
# Stream should also be defined as a factor
invert$StreamF <- factor(invert$Stream)

# It turns out that missing values cause subtle problems in some R
# functions that deal with the fitted object. Sigh... Usually, R
# is sensible enough, but not all package authors did a good job.
dim(invert)
invert <- invert[complete.cases(invert),]
dim(invert)
invert[1:12,]
##***part001e;
sink()

str(invert) # check the structure


# look at the std dev to mean plot 
# Compute the averages etc
sink('invert-R-020.txt', split=TRUE)
##***part020b;
mean.invert <- ddply(invert, c("Year","YearF","Stream","StreamF","Period"), function(x){
  # compute sample size, missing values, mean, and sd
  n <- nrow(x)
  n.miss <- sum(is.na(x$count))
  count.mean <- mean(x$count, na.rm=TRUE)
  count.sd   <- sd  (x$count, na.rm=TRUE)
  res <- c(n=n, n.miss=n.miss, count.mean=count.mean, count.sd=count.sd) # put results together
  res
  })
mean.invert
##***part020e;
sink()


# Plot the std vs.  mean
# Because there is no relatinship between the mean and the sd,
# This plot shows that indicates that no transformation is needed.
##***part060b
sd.plot <- ggplot(data=mean.invert, aes(x=log(count.mean), y=log(count.sd), 
                                        shape=StreamF, color=StreamF))+
  ggtitle("SD vs. Mean")+
  geom_point(size=4)
sd.plot
##***part060e;

ggsave(plot=sd.plot, file='invert-R-060-varmeanplot.png', height=4, width=6, units="in", dpi=300)



# Plot the mean count over time by stream
##***part070b;
plot.mean <- ggplot(data=invert, aes(x=Year, y=count, 
                    shape=StreamF, group=StreamF, color=StreamF))+
  ggtitle("Preliminary plot of data and means")+
  geom_point(size=2, position=position_dodge(w=0.2))+
  geom_vline(xintercept=.5+max(invert$Year[as.character(invert$Period)=="Before"],na.rm=TRUE))+
  stat_summary(fun.y="mean", geom="line") # , position=position_dodge(w=0.2))
plot.mean
##***part070e;


ggsave(plot=plot.mean, file='invert-R-070-trendplot.png', height=4, width=6, units='in', dpi=300)




############################################################################################

# Analysis of Stream 1 alone  to illustrate how to analyze one stream's data
stream1 <- invert[ invert$Stream ==1,]
stream1


# The analysis of the mean(Count)) for each year
mean.invert1 <- mean.invert[ mean.invert$Stream ==1, ]
mean.invert1 


# Test for a period effect using the mean count data
sink('invert-R-100.txt', split=TRUE)
##***part100b;
result100 <- lm( count.mean ~ Period, data=mean.invert1)
anova(result100)
##***part100e;
sink()


# Estimate the period effect
# Do NOT use thesummary output because it gives odd results
# with ordered factors. Compare the two following models summary output.
# They should give identical estimates but are not even though the p-value is the same
summary(result100) #with ordered factor
summary(lm(count.mean ~ factor(as.character(Period)), data=mean.invert1))
# It is better to formally estimate the difference using the
# lsmeans package and use the pairs to estimate the difference
sink('invert-R-105.txt', split=TRUE)
##***part105b;
result100.lsmo <- lsmeans::lsmeans(result100, ~Period)
cat("\n\n Estimated marginal means for each Period \n\n")
summary(result100.lsmo)
cat("Estimated difference between means in Before and After periods\n")
confint(pairs(result100.lsmo, infer=TRUE))
##***part105e;
sink()



# Residual and other plots

##***part106b;
plot.diag100 <- sf.autoplot.lm(result100)
plot.diag100
##***part106e;
ggsave(plot=plot.diag100, file='invert-R-106.png', height=4, width=6, units="in", dpi=300)




#################################################################################
# Analysis of the individual quadrat data for Stream 1.
# 

sink('invert-R-200-type3.txt', split=TRUE)
##***part200b;
# Don't forget that you need a Year as a factor.
# We created YearF as a factor when we readin the data
# We don't need to transform counts because they are largish.
result200 <- lmer(count ~ Period + (1|YearF), data=stream1)
anova(result200, ddf="Kenward-Roger")
##***part200e;
sink()

# Again, don't trust the estimates from the summary table.
summary(result200)


sink('invert-R-200-vc.txt', split=TRUE)
##***part200-vcb;
# Extract the variance components.
vc <- VarCorr(result200)
vc
##***part200-vce;
sink()


# LSmeans after a lmer() fit
sink('invert-R-200LSM-Period.txt', split=TRUE)
##***part200LSM-Periodb;
# Need to use the lsmeans::lsmeans() notation because there is a
# lsmeans() function in both the lsmeans and lmerTest package.
result200.lsmo <- lsmeans::lsmeans(result200, ~Period)
summary(result200.lsmo)
##***part200LSM-Periode;
sink()


sink("invert-R-200baci.txt", split=TRUE)
##***part200bacib; 
# Estimate the period effect along with a se
cat("Estimated difference between means in Before and After periods\n")
confint(pairs(result200.lsmo, infer=TRUE))
##***part200bacie;
sink()


# Diagnostic plots for mixed models are not as standardized as for
# ordinary linear models so there are several than can be produced.
##***part200diagnosticb;
diag.plot200 <- sf.autoplot.lmer(result200)
diag.plot200
##***part200diagnostice;

ggsave(plot=diag.plot200, file='invert-R-200-diagnostic.png', height=4, width=6, units="in", dpi=300)






#--------------------------------------------------- Analyze all streams together --------------



# The analysis of the means values



sink('invert-R-300-type3.txt', split=TRUE)
##***part300b;
# Note that Stream, Period, and Year all need to be factors
# These were defined as such when the data were read in.
result300 <- lmer(count.mean~ Period + (1|Stream) + (1|YearF), data=mean.invert)
anova(result300, ddf='Kenward-Roger')
##***part300e;
sink()

# Again be careful with the summary table because Period is an ordered factor
summary(result300)


sink('invert-R-300-vc.txt', split=TRUE)
##***part300-vcb;
# Extract the variance components
vc <-VarCorr(result300)
vc
##***part300-vce;
sink()


# Estimate the marginal means
sink('invert-R-300LSM-Period.txt', split=TRUE)
##***part300LSM-Periodb;
# Need to use the lsmeans::lsmeans() notation because there is a
# lsmeans() function in both the lsmeans and lmerTest package.
result300.lsmo <- lsmeans::lsmeans(result300, ~Period)
cat("\n\n Estimated marginal means for each Period \n\n")
summary(result300.lsmo)
##***part300LSM-Periode;
sink()


sink("invert-R-300baci.txt", split=TRUE)
##***part300bacib;
# Estimate the Period effect
cat("Estimated difference between means in Before and After periods\n")
pairs(result300.lsmo)
confint(pairs(result300.lsmo))
##***part300bacie;
sink()




# Check the lmer residuals 
##***part300diagnosticb;
diag.plot300 <- sf.autoplot.lmer(result300)
diag.plot300
##***part300diagnostice;

ggsave(plot=diag.plot300, file='invert-R-300-diagnostic.png', height=4, width=6, units="in", dpi=300)




#######################################################################################
# Now for the analysis of the entire dataset (whew)


sink('invert-R-400-type3.txt', split=TRUE)
##***part400b;
# Year, Period, Stream must all be defined as factors.
# This was done when the data were read in.
result400 <- lmer(count ~ Period + (1|StreamF)+(1|YearF)+(1|StreamF:YearF), data=invert)
anova(result400, ddf="Kenward-Roger")
##***part400e;
sink()

# Again be very careful about using the results
# from the summary() because Period is defined as an ordered factor
summary(result400)


sink('invert-R-400-vc.txt', split=TRUE)
##***part400-vcb;
# Extract the variance components
vc <- VarCorr(result400)
vc
##***part400-vce;
sink()




sink('invert-R-400LSM-Period.txt', split=TRUE)  # Note these have the wrong standard error
##***part400LSM-Periodb;
# Need to use the lsmeans::lsmeans() notation because there is a
# lsmeans() function in both the lsmeans and lmerTest package.
result400.lsmo <- lsmeans::lsmeans(result400, ~Period)
cat("\n\n Estimated marginal means for each Period \n\n")
summary(result400.lsmo)
##***part400LSM-Periode;
sink()


sink("invert-R-400baci.txt", split=TRUE)  # note these have the wrong df
##***part400bacib; 
# Estimate the Period effect
cat("Estimated difference between means in Before and After periods\n")
pairs(result400.lsmo)
confint(pairs(result400.lsmo))
##***part400bacie;
sink()


# diagnostic plots for the lmer fit

##***part400diagnosticb;
diag.plot400 <- sf.autoplot.lmer(result400)
diag.plot400
##***part400diagnostice;

ggsave(plot=diag.plot400, file='invert-R-400-diagnostic.png', height=4, width=6, units="in", dpi=300)





