# Systolic Blood Pressure under a Stocking of conditions.
# Dataset from Claire Protheroe, BPK.
# Split-plot with main plots in blocks; pseudo-replication at sub-plot level
#   and missing data at the sub-plot level
# 2013-03-13 CJS First edition
   
# The file includes data for systolic blood pressure measures in 
# three different tests. The fifteen subjects took part in all three tests 
# so we have a paired data set. However, for each individual, some tests were 
# longer than others, so there are different numbers of data points for 
# each individual for each of the three tests.


# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.


options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected


# Read in the data
##---part000b;
sink('SystolicBloodPressure-R-000.txt', split=TRUE)
in.sysbp <- read.csv('SystolicBloodPressure.csv', header=TRUE)
in.sysbp$Subject   <- factor(in.sysbp$Subject)
in.sysbp$Stocking  <- factor(in.sysbp$Stocking)
in.sysbp$Time      <- factor(in.sysbp$Time)
in.sysbp$Phase     <- factor(in.sysbp$Phase)
cat('*** Part of the raw data *** \n\n')
in.sysbp[1:5,]
##---part000e;
sink()

sink('SystolicBloodPressure-R-000-exclude.txt', split=TRUE)
##---part000-excludeb;
# Drop some phases that do not have good data
cat("Dimensions of data BEFORE exclusions: ",dim(in.sysbp), "\n")
in.sysbp <- in.sysbp[! grepl('-60',in.sysbp$Phase),]
cat("Dimensions of data AFTER exclusions: ",dim(in.sysbp), "\n")


# Notice that now the Phase factor has an extra level (the -60 level) that causes
# problems in future analyses. We need to drop unused levels by doing another factor() command ... sigh...
in.sysbp$Subject   <- factor(in.sysbp$Subject)
in.sysbp$Stocking <- factor(in.sysbp$Stocking)
in.sysbp$Time      <- factor(in.sysbp$Time)
in.sysbp$Phase     <- factor(in.sysbp$Phase)


# Average over the pseudoreplicates
library(doBy)
sysbp <- summaryBy( SysBP ~ Stocking +  Phase + Subject, data=in.sysbp)
names(sysbp)[names(sysbp)=="SysBP.mean"] <- 'MeanSysBP'
cat("Average over the pseudoreplicates\n")
sysbp[1:5,]
#dim(sysbp)
##---part000-excludee;
sink()


# We need to remove blanks and periods and - signs from level names as this causes problems 
# later on in the cld() functions - sigh..R is free but not cheap
# See http://stats.stackexchange.com/questions/31547/
#     how-to-obtain-the-results-of-a-tukey-hsd-post-hoc-test-in-a-table-showing-groupe
sysbp$Stocking <- factor(chartr(" -", "XX", sysbp$Stocking))
sysbp$Phase     <- factor(chartr(" -", "XX", sysbp$Phase))
sysbp$Subject   <- factor(chartr(" -", "XX", sysbp$Subject))
sysbp$Trt       <- with(sysbp, interaction(Stocking,Phase))
str(sysbp)


# Get side by side dot plots for each treatment
# Get side-by-side dot plots
png(file="SystolicBloodPressure-R-001.png")  # send the plot to a png file
##---part001b;
boxplot( MeanSysBP ~ Stocking:Phase, data=sysbp, range=0,
   notch=FALSE, # helps to compare pop means
   main="Mean systolic blood pressure", 
   sub='Whiskers extend to range of means',
   xlab='Stocking:Phase', ylab='Mean Sys BP')

stripchart(MeanSysBP ~ Stocking:Phase, data=sysbp, add=TRUE, 
     vertical=TRUE, method="jitter", jitter=.1)
##---part001e;
dev.off()

# There is no easy easy way to make a nice report showing summary
# statistics by group in R unless you install some of the packages
# such as the doBy package.
# Compute some summary statistics for each group
sink('SystolicBloodPressure-R-002.txt', split=TRUE)
##---part002b;
library(doBy)
# We don't compute standard errors because design is NOT a CRD
report<- summaryBy(MeanSysBP ~ Stocking+Phase, data=sysbp, FUN=c(length,mean,sd))
cat("*** Summary of raw means and standard deviations *** \n\n")
report
##---part002e;
sink()

# Plot the std deviation vs the mean to see a transformation is needed
with(report, plot(log(MeanSysBP.mean),log(MeanSysBP.sd), main='SD vs Mean'))
# There is osme evidence of a increase in the SD with the mean, but it doesn't
# appear to be too serious


png(file='SystolicBloodPressure-R-003-profile-subject.png')
##---part003-profile-subjectb;
stripchart( MeanSysBP ~ Trt, data=sysbp, vertical=TRUE,
      main='Profile plot of each subject', ylim=range(sysbp$MeanSysBP)+c(-5, 5),
      xlab='Phase + Stocking)', ylab='Mean Systolick Blood Pressure')
invisible(lapply(split(sysbp, sysbp$Subject), 
          function(x){x <- x[order(x$Trt),];lines(x$Trt, x$MeanSysBP)}))
##---part003-profile-subjecte;
dev.off()






#------------------------------------------------------------------------------------
# Classical analysis using aov() function
sink('SystolicBloodPressure-R-010.txt', split=TRUE)
##---part010b;
sysbp.fit.aov <- aov( MeanSysBP ~ Phase * Stocking + Error(Subject/Stocking), data = sysbp)
cat("*** Results from classic aov() fit to the design ***\n\n")
summary(sysbp.fit.aov)
##---part010e;
sink()






#---------------------------------------------------------------------------------------
# Now for the analysis using lme in the nlme package

##---part020b;
library(nlme)
# Check that all relevant variables are factors
str(sysbp)
sysbp.fit <- lme(fixed=MeanSysBP ~ Stocking + Phase + Stocking:Phase,
     random=~ 1 | Subject / Stocking,
     data=sysbp)
##---part020e;

sink('SystolicBloodPressure-R-021.txt', split=TRUE)
##---part021b;     
# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# for details.
cat("*** Tests for fixed effects from lme(). ***\n",
    "    These are conditional on the variance component estimates ***\n\n")
anova(sysbp.fit)
##---part021e;
sink()

sink('SystolicBloodPressure-R-022.txt', split=TRUE)
##---part022b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
summary(sysbp.fit)
nlme::VarCorr(sysbp.fit)  # Need to override lmer() VarCOrr
##---part022e;
sink()

sink('SystolicBloodPressure-R-023.txt', split=TRUE)
##---part023b;
# Get the lsmeans for each level using the doBy package=, except it doesn't work
# with the interaction effect.

library(doBy)
popMeans(sysbp.fit, effect="Phase")
popMeans(sysbp.fit, effect="Stocking")
popMeans(sysbp.fit, effect="Stocking:Phase")  # This doesn't work = groan 


# We load the lsmeans package. It does the lsmeans for lme objects
# but the df is NOT specified (groan) and you need to specify by hand
library(lsmeans)
lsmeans(sysbp.fit,   list(~Stocking))
lsmeans(sysbp.fit,   list(~Phase))
lsmeans(sysbp.fit,   list(~Stocking:Phase))

# Now for the comparisons between all pairs of lsmeans
# Again notice that df are not automatically computed and you must specify by hand
# This is the df for denominators for F tests of main effects, but is
# difficult to specify for interaction terms.
# Notice that if you use the same functions with lmer() it will compute
# the df using the KR approximation.
lsmeans(sysbp.fit,   list(pairwise~Stocking), adjust="none" ) # No adjustment for MC
lsmeans(sysbp.fit,   list(pairwise~Stocking), adjust="tukey") # no df computed
lsmeans(sysbp.fit,   list(pairwise~Stocking), adjust="tukey", glhargs=list(df=30))
lsmeans(sysbp.fit,   list(pairwise~Phase),     adjust="tukey", glhargs=list(df=90))
lsmeans(sysbp.fit,   list(pairwise~Stocking:Phase), adjust="tukey", glhargs=list(df=90))
sink()


png('SystolicBloodPressure-R-025.png')
sink('SystolicBloodPressure-R-025.txt', split=TRUE)
##---part025b;
# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!

library(multcomp)
cat("*** Multiple comparison for Phases *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Phase.mc <-glht(sysbp.fit, linfct=mcp(Phase="Tukey", interaction_average=TRUE) )
summary(Phase.mc) 
confint(Phase.mc)


cat("*** Multiple comparison for Phases ***\n",
    "    but with correct degrees of freedom *** \n\n")
Phase.mc <-glht(sysbp.fit, linfct=mcp(Phase="Tukey", interaction_average=TRUE), df=90 )
summary(Phase.mc) 
confint(Phase.mc)
plot(Phase.mc)

# compact letter display (the joined line plots)
Phase.mc.cld <- cld(Phase.mc)
Phase.mc.cld

##---part025e;
sink()
dev.off()

png('SystolicBloodPressure-R-026.png')
sink('SystolicBloodPressure-R-026.txt', split=TRUE)
##---part026b;
Stocking.mc <- glht(sysbp.fit, linfct=mcp(Stocking="Tukey",interaction_average=TRUE),df=30)
cat("*** Multiple comparions for Stocking ***\n",
    "    with correct df \n\n")
summary(Stocking.mc)
confint(Stocking.mc)
plot(Stocking.mc)

Stocking.mc.cld <- cld(Stocking.mc)
Stocking.mc.cld
##---part026e;
sink()
dev.off()


sink('SystolicBloodPressure-R-027.txt', split=TRUE)
##---part027b;
# In order to get the MCP on the Stocking:Phase combinations, we need to refit
# The model using a pseudofactor
sysbp$TP <- interaction(sysbp$Stocking, sysbp$Phase)
sysbp.fit2 <- lme(fixed=MeanSysBP ~ TP,
     random=~ 1 | Subject / Stocking,
     data=sysbp)
anova(sysbp.fit2)
summary(sysbp.fit2)
##---part027e;
sink()

png('SystolicBloodPressure-R-027b.png')
sink('SystolicBloodPressure-R-027b.txt', split=TRUE)
##---part027bb;
# Note the different standard errors for the comparison depending if the comparison
# is between Stocking:Phase combinations within or outside the same subject.
# There is no easy way to determine the df for some of the comparisons and they differ depending
# if the comparison is inter- or intra-subject. To be safe, we will use the smaller df corresponding
# to inter-subjectk comparisons () which needs to be done by hand (or look at the SAS output)
Stocking.Phase.mc <- glht(sysbp.fit2, linfct=mcp(TP="Tukey"), df=90)

cat("*** Multiple comparisons for Stocking:Phase ***\n",
    "    The df are not easily determined as they vary if these are\n",
    "    intra- or inter-subject comparisons. Use a conservative value\n\n")
summary(Stocking.Phase.mc)
confint(Stocking.Phase.mc)
plot(Stocking.Phase.mc)

Stocking.Phase.mc.cld <- cld(Stocking.Phase.mc)
Stocking.Phase.mc.cld
##---part027be;
sink()
dev.off()

png('SystolicBloodPressure-R-028.png')
##---part028b;
# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(sysbp.fit)
plot2 <- plot(sysbp.fit, main="Residual plot")
plot3 <- plot(sysbp.fit, MeanSysBP ~ fitted(.), abline=c(0,1),
   main="Observed vs Predicted")
print(plot1, position=c(0,.5,.5, 1), more=TRUE)
print(plot2, position=c(0, 0,.5, .5), more=TRUE)
print(plot3, position=c(.5,0, 1, .5))
##---part028e;
dev.off()






###-------------------------- Using lmer() function  ------------------
# The lmer() function provides a much simple way to specify 
# models with random effect. HOWEVER, the ordinary lmer() function
# does NOT produce p-values.
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
#     for details on why LMER does NOT produce p-values.
# Also see http://glmm.wikidot.com/faq for further information.

# Fortunately, the lmerTest package has recently been produced which 
# adds p-values based on a Satterthaite approximation 
#
# You may nave to detach all packages and reload packages as there are name conflicts
# among the packages
#

names(sessionInfo()$otherPkgs) 
detach(package:nlme)  # it interferes with lmerTest
detach(package:lsmeans) # it interferes also with lmerTest
library(lmerTest)  # computed p-values for lmer() calls - hurrah


# Check that all relevant variables are factors
str(sysbp)

##---part300b;
sysbp.fit.lmerTest <- lmer(MeanSysBP ~ Stocking + Phase + Stocking:Phase +
     (1 | Subject) + (1 | Subject:Stocking),
     data=sysbp)
##---part300e;
# This could also be specified using the Subject / Stocking syntax for the random effects
# and you get the identical model, i.e.
#sysbp.fit.lmer <- lmer(MeanSysBP ~ Stocking + Phase + Stocking:Phase +
#     (1 | Subject / Stocking), data=sysbp)


sink('SystolicBloodPressure-R-310.txt', split=TRUE)
##---part310b;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(sysbp.fit.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(sysbp.fit.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(sysbp.fit.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##---part310e;
sink()


# Summary table
summary(sysbp.fit.lmerTest)

# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff='Stocking')
lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff='Phase')
lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff='Stocking:Phase') # LOTS of output!

plot(lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff="Stocking"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff="Phase"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(sysbp.fit.lmerTest, test.eff="Stocking:Phase"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed



# Estimate the difference in the lsmeans. Notice that NO adjustment has been made
# for multiple comparisions, i.e. no Tukey adjustment
diff.Stocking <- lmerTest::difflsmeans(sysbp.fit.lmerTest, test.eff="Stocking")
print(diff.Stocking)
plot(diff.Stocking)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed


diff.Phase <- lmerTest::difflsmeans(sysbp.fit.lmerTest, test.eff="Phase")
print(diff.Phase)
plot(diff.Phase)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed


# Note the different standard errors for the comparison of Stocking:Phase terms depending if the comparison
# is between Stocking:Phase combinations within or outside the same subject.

diff.Stocking.Phase <- lmerTest::difflsmeans(sysbp.fit.lmerTest, test.eff="Stocking:Phase")
#print(diff.Stocking.Phase) # LOTS of output -- too much to print here
plot(diff.Stocking.Phase)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed




# The lsmeans() package also does these types of comparisons
# Now it computes the approximate df using the KR approximation - hurrah
# You have to be careful to detach some of the earlier packages because of name conflicts
# There is no compact letter display though and no plotting method for its output - bummer

names(sessionInfo()$otherPkgs) 
detach(package:lmerTest) # go back to regular lmer
library(lme4)

# We need to refit WITHOUT lmerTest so that the class of the package is correct
sysbp.fit.lmer <- lme4::lmer(MeanSysBP ~ Stocking + Phase + Stocking:Phase +
     (1 | Subject) + (1 | Subject:Stocking),
     data=sysbp)

library(lsmeans) 
#


lsmeans::lsmeans(sysbp.fit.lmer,   list(pairwise~Stocking), adjust="none") 
diff.Stocking2 <- lsmeans::lsmeans(sysbp.fit.lmer,   list(pairwise~Stocking), adjust="tukey")

sink('SystolicBloodPressure-R-331-phase.txt', split=TRUE)
##---part331-phaseb;
lsmeans::lsmeans(sysbp.fit.lmer,   list(pairwise~Phase),   adjust="tukey")
##---part331-phasee;
sink()
diff.Phase2 <- lsmeans::lsmeans(sysbp.fit.lmer,   list(pairwise~Phase),   adjust="tukey") #get the df for later



#lsmeans::lsmeans(sysbp.fit.lmer,   list(pairwise~Stocking:Phase), adjust="tukey")


# Multiple comparisons using MULTCOMP

# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!


library(multcomp)
cat("*** Multiple comparison for Phases *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Phase.mc <-glht(sysbp.fit.lmer, linfct=mcp(Phase="Tukey", interaction_average=TRUE) )
summary(Phase.mc) 
confint(Phase.mc)

sink('SystolicBloodPressure-R-331-phase-cld.txt', split=TRUE)
##---part331-phase-cldb;
cat("*** Multiple comparison for Phases ***\n",
    "    but with correct degrees of freedom *** \n\n")
Phase.mc <-glht(sysbp.fit.lmer, linfct=mcp(Phase="Tukey", interaction_average=TRUE), 
                df=min(round(diff.Phase2$"Phase pairwise differences"$df[1])))
summary(Phase.mc) 
confint(Phase.mc)
plot(Phase.mc)

# compact letter display (the joined line plots)
Phase.mc.cld <- cld(Phase.mc)
Phase.mc.cld
##---part331-phase-clde;
sink()


Stocking.mc <- glht(sysbp.fit.lmer, linfct=mcp(Stocking="Tukey",interaction_average=TRUE),
                    df=min(round(diff.Stocking2$"Stocking pairwise differences"$df[1])))
cat("*** Multiple comparions for Stocking ***\n",
    "    with correct df \n\n")
summary(Stocking.mc)
confint(Stocking.mc)
plot(Stocking.mc)

Stocking.mc.cld <- cld(Stocking.mc)
Stocking.mc.cld


# In order to get the MCP on the Stocking:Phase combinations, we need to refit
# The model using a pseudofactor
sysbp$TP <- interaction(sysbp$Stocking, sysbp$Phase)
sysbp.fit.lmer2 <- lmer(MeanSysBP ~ -1 + TP + (1 | Subject) + (1 | Subject:Stocking),
     data=sysbp)
anova(sysbp.fit.lmer2, data=sysbp)
summary(sysbp.fit.lmer2)


# Note the different standard errors for the comparison depending if the comparison
# is between Stocking:Phase combinations within or outside the same subject.
# There is no easy way to determine the df for some of the comparisons and they differ depending
# if the comparison is inter- or intra-subject. To be safe, we will use the smaller df corresponding
# to inter-subject comparisons () which needs to be done by hand (or look at the SAS output)

Stocking.Phase.mc <- glht(sysbp.fit.lmer2, linfct=mcp(TP="Tukey"), df=90)

cat("*** Multiple comparisons for Stocking:Phase ***\n",
    "    The df are not easily determined as they vary if these are\n",
    "    intra- or inter-subject comparisons. Use a conservative value\n\n")
summary(Stocking.Phase.mc)
confint(Stocking.Phase.mc)
plot(Stocking.Phase.mc)

Stocking.Phase.mc.cld <- cld(Stocking.Phase.mc)
Stocking.Phase.mc.cld





# Extract the variance components
vc <- VarCorr(sysbp.fit.lmer)
as.matrix(vc)

# Or look at the slots directly
slotNames(summary(sysbp.fit.lmer))
data.frame(summary(sysbp.fit.lmer)@REmat)



png(file="SystolicBloodPressure-R-500-diagnostics.png")
##---part500-diagnosticsb;
# diagnostic plots.
# get the residual plot and normal probability plot

layout(matrix(1:4, 2,2))
qqnorm(resid(sysbp.fit.lmer),main="Q-Q plot for residuals")

plot(resid(sysbp.fit.lmer) ~ fitted(sysbp.fit.lmer),main="residual plot")
abline(h=0)

qqnorm(ranef(sysbp.fit.lmer)$Subject$"(Intercept)", main="Q-Q plot for the random BLOCK effect" )
qqnorm(ranef(sysbp.fit.lmer)$'Subject:Stocking'$"(Intercept)", main="Q-Q plot for the random Subject:Stocking  effect" )
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed
##---part500-diagnosticse;
dev.off()
