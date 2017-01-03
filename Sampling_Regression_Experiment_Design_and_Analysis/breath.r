# Two factor split-plot; Ancova; Random intercept model

# 2013-08-28 CJS First version created.

# Holding your breath at different water temperatures.

# This dataset was provided Matthew D. White in BPK at SFU.
# Thesis C121W: A thermosensitive sodium channel mutation''. 
#  Additional # details are available at
#  http://dx.doi.org/10.1016/j.bpj.2010.12.2506.

# Breath holding times.
# How does the time that a subject can hold its breath vary by the temperature of the water
# in which you are immersed. Does it vary between males and females?

#  Several subjects of each sex were asked to hold their breath when immersed in
# water of various temperatures. The time (seconds) was recorded.

#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected


# Read in the data and list part of the data
sink("breath-R-000.txt", split=TRUE)
##---part000b;
breath <- read.csv('breath2.csv', header=TRUE)
cat('*** Part of the raw data *** \n\n')
# Make sure that various things are factors
breath$Gender   <- as.factor(breath$Gender)
breath$Subject  <- as.factor(breath$Subject)
breath$Temp     <- as.factor(breath$Temp)
breath$Trt      <- with(breath, interaction(Gender,Temp))
breath[1:10,]
##---part000e;
sink()


# Get side by side dot plots for each treatment
# Get side-by-side dot plots
png(file="breath-R-001.png")  # send the plot to a png file
##---part001b;
boxplot( Time ~ Gender:Temp, data=breath, range=0,
   notch=FALSE, # helps to compare pop means
   main="Temp to hold breath", 
   sub='Whiskers extend to range of data',
   xlab='Gender:Temp', ylab='Temp')

stripchart(Time ~ Gender:Temp, data=breath, add=TRUE, 
     vertical=TRUE, method="jitter", jitter=.1)
##---part001e;
dev.off()


# Compute the means and standard deviations and sample sizes for
# each treatment group
# There is no easy easy way to make a nice report showing summary
# statistics by group in R unless you install some of the packages
# such as the doBy package. 

sink('breath-R-002.txt')
##---part002b;
library(doBy)
report<- summaryBy(Time ~ Gender + Temp, data=breath, 
         FUN=c(length,mean,sd))
report
##---part002e;
sink()

# Plot the standard deviation vs the mean to see if there
# is a relationship
png('breath-R-003-SDvsMean.png')
##---part003-SDvsMeanb;
plot(log(report$Time.mean), log(report$Time.sd),  
   main='Standard deviation vs the mean')
##---part003-SDvsMeane;
dev.off()


# Create a profile plot of the subject responses over time

png('breath-R-003-profile-subject.png')
##---part003-profile-subjectb;
plot( as.numeric(as.character(breath$Temp)), breath$Time, type="p",
      main='Profile plot of each subject', ylim=range(breath$Time)+c(-5, 5),
      xlab='Temperature', ylab='Time holding breath')
text( as.numeric(as.character(breath$Temp))+.5, breath$Time, labels=breath$Subject, pos=3)
invisible(lapply(split(breath, breath$Subject), function(x){lines(as.numeric(as.character(x$Temp)), x$Time)}))
##---part003-profile-subjecte;
dev.off()


##---part003-removeb;
# Remove subjects 3 and 6
breath_nooutlier <- breath[breath$Subject !=3 & breath$Subject !=6,]
##---part003-removee;

# Make the profile plot after removing outliers

png('breath-R-003-profile-subject2.png')
##---part003-profile-subject2b;
plot( as.numeric(as.character(breath_nooutlier$Temp)), breath_nooutlier$Time, type="p",
      main='Profile plot of each subject', ylim=range(breath_nooutlier$Time)+c(-5, 5),
      xlab='Temperature', ylab='Time holding breath')
text( as.numeric(as.character(breath_nooutlier$Temp))+.5, breath_nooutlier$Time, labels=breath_nooutlier$Subject, pos=3)
invisible(lapply(split(breath_nooutlier, breath_nooutlier$Subject), function(x){lines(as.numeric(as.character(x$Temp)), x$Time)}))
##---part003-profile-subject2e;
dev.off()


#---------------------------------------------------------------------------------------

# Classical analysis using aov() function
sink('breath-R-010.txt', split=TRUE)
##---part010b;
# check that all relevant variables are factors
str(breath)
breath.fit.aov <- aov( Time ~ Gender * Temp + Error(Subject), data=breath_nooutlier)
cat("*** Results from classic aov() fit to the design ***\n\n")
summary(breath.fit.aov)
##---part010e;
sink()


#----------------------------------------------------------------------------------------------
# Now for the analysis using lme in the nlme package

##---part020b;
library(nlme)
# Check that all relevant variables are factors
str(breath)
breath.fit <- lme(fixed=Time ~ Temp + Gender + Temp:Gender,
     random=~ 1 | Subject,
     data=breath_nooutlier)
##---part0203;

sink('breath-R-021.txt', split=TRUE)
##---part021b;     
# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# for details.
cat("*** Tests for fixed effects from lme(). ***\n",
    "    These are conditional on the variance component estimates ***\n\n")
anova(breath.fit)
##---part021e;
sink()

sink('breath-R-022.txt', split=TRUE)
##---part022b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
summary(breath.fit)
nlme::VarCorr(breath.fit)  # Need to override lmer() VarCOrr
##---part022e;
sink()

sink('breath-R-023.txt', split=TRUE)
##---part023b;
# Get the lsmeans for each level using the doBy package=, except it doesn't work
# with the interaction effect.

library(doBy)
popMeans(breath.fit, effect="Gender")
popMeans(breath.fit, effect="Temp")
popMeans(breath.fit, effect="Temp:Gender")  # This doesn't work = groan 


# We load the lsmeans package. It does the lsmeans for lme objects
# but the df is NOT specified (groan) and you need to specify by hand
library(lsmeans)
lsmeans(breath.fit,   list(~Temp))
lsmeans(breath.fit,   list(~Gender))
lsmeans(breath.fit,   list(~Temp:Gender))

# Now for the comparisons between all pairs of lsmeans
# Again notice that df are not automatically computed and you must specify by hand
# This is the df for denominators for F tests of main effects, but is
# difficult to specify for interaction terms.
# Notice that if you use the same functions with lmer() it will compute
# the df using the KR approximation.
lsmeans(breath.fit,   list(pairwise~Temp), adjust="none" ) # No adjustment for MC
lsmeans(breath.fit,   list(pairwise~Temp), adjust="tukey") # no df computed
lsmeans(breath.fit,   list(pairwise~Temp), adjust="tukey", glhargs=list(df=24))
lsmeans(breath.fit,   list(pairwise~Gender),   adjust="none", glhargs=list(df=8))
lsmeans(breath.fit,   list(pairwise~Temp:Gender), adjust="tukey", glhargs=list(df=24))
sink()


png('breath-R-025.png')
sink('breath-R-025.txt', split=TRUE)
##---part025b;
# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!

library(multcomp)
cat("*** Multiple comparison for Genders *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit, linfct=mcp(Gender="Tukey", interaction_average=TRUE) )
summary(Gender.mc) 
confint(Gender.mc)


cat("*** Multiple comparison for Genders ***\n",
    "    but with correct degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit, linfct=mcp(Gender="Tukey", interaction_average=TRUE), df=8 )
summary(Gender.mc) 
confint(Gender.mc)
plot(Gender.mc)

# compact letter display (the joined line plots)
Gender.mc.cld <- cld(Gender.mc)
Gender.mc.cld

##---part025e;
sink()
dev.off()

png('breath-R-026.png')
sink('breath-R-026.txt', split=TRUE)
##---part026b;
Temp.mc <- glht(breath.fit, linfct=mcp(Temp="Tukey",interaction_average=TRUE),df=24)
cat("*** Multiple comparions for Temp ***\n",
    "    with correct df \n\n")
summary(Temp.mc)
confint(Temp.mc)
plot(Temp.mc)

Temp.mc.cld <- cld(Temp.mc)
Temp.mc.cld
##---part026e;
sink()
dev.off()


sink('breath-R-027.txt', split=TRUE)
##---part027b;
# In order to get the MCP on the Temp:Gender combinations, we need to refit
# The model using a pseudofactor
breath_nooutlier$TG <- interaction(breath_nooutlier$Temp, breath_nooutlier$Gender)
breath.fit2 <- lme(fixed=Time ~  TG,
     random=~ 1 | Subject,
     data=breath_nooutlier)
anova(breath.fit2)
summary(breath.fit2)
##---part027e;
sink()

png('breath-R-027b.png')
sink('breath-R-027b.txt', split=TRUE)
##---part027bb;
# Note the different standard errors for the comparison depending if the comparison
# is between Temp:Gender combinations within or outside the same Subject.
# There is no easy way to determine the df for some of the comparisons and they differ depending
# if the comparison is inter or intra site. To be safe, we will use the smaller df corresponding
# to inter-site comparisons () which needs to be done by hand (or look at the SAS output)
Temp.Gender.mc <- glht(breath.fit2, linfct=mcp(TG="Tukey"), df=24)

cat("*** Multiple comparisons for Temp:Gender ***\n",
    "    The df are not easily determined as they vary if these are\n",
    "    intra- or inter-site comparisons. Use a conservative value\n\n")
summary(Temp.Gender.mc)
confint(Temp.Gender.mc)
plot(Temp.Gender.mc)

Temp.Gender.mc.cld <- cld(Temp.Gender.mc)
Temp.Gender.mc.cld
##---part027be;
sink()
dev.off()

png('breath-R-028.png')
##---part028b;
# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(breath.fit)
plot2 <- plot(breath.fit, main="Residual plot")
plot3 <- plot(breath.fit, Time ~ fitted(.), abline=c(0,1),
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
str(breath)

##---part300b;
breath.fit.lmerTest <- lmerTest::lmer(Time ~ Temp + Gender + Temp:Gender +
     (1 | Subject),
     data=breath_nooutlier)
##---part300e;


sink('breath-R-310.txt', split=TRUE)
##---part310b;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(breath.fit.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(breath.fit.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(breath.fit.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##---part310e;
sink()

# Summary table
summary(breath.fit.lmerTest)

sink('breath-R-320.txt', split=TRUE)
##---part320b;
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
lmerTest::lsmeans(breath.fit.lmerTest, test.eff='Temp')
lmerTest::lsmeans(breath.fit.lmerTest, test.eff='Gender')
GT.means <- lmerTest::lsmeans(breath.fit.lmerTest, test.eff='Temp:Gender') # LOTS of output!

plot(lmerTest::lsmeans(breath.fit.lmerTest, test.eff="Temp"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(breath.fit.lmerTest, test.eff="Gender"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(breath.fit.lmerTest, test.eff="Temp:Gender"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed
##---part320e;
sink()


png("breath-R-370.png")
##---part370b;
# Create plot of the fitted GT values along with confidence limits for each
plot(as.numeric(as.character(GT.means$lsmeans.table$Temp))+.5*(GT.means$lsmeans.table$Gender=="Female"),
                GT.means$lsmeans.table$Estimate,
                ylim=range(GT.means$lsmeans.table[,c("Lower CI","Upper CI")]),
                ylab="Predicted Breath Holding Time",
                xlab='Temperature',
                main="Predicted breath holding times with 95% ci")
invisible(lapply(split(GT.means$lsmeans.table, GT.means$lsmeans.table$Gender),
          function(x){
            lines(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"),x$Estimate, lty=1+(x$Gender=="Female")[1])
            segments(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Lower CI",
                     as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Upper CI")
          }))
##---part370e;
dev.off()


sink('breath-R-330.txt', split=TRUE)
##---part330b;
# Estimate the difference in the lsmeans. Notice that NO adjustment has been made
# for multiple comparisions, i.e. no Tukey adjustment
diff.Gender <- lmerTest::difflsmeans(breath.fit.lmerTest, test.eff="Gender")
print(diff.Gender)
plot(diff.Gender)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed
##---part330e;
sink()


diff.Temp <- lmerTest::difflsmeans(breath.fit.lmer, test.eff="Temp")
print(diff.Temp)
plot(diff.Temp)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed


# Note the different standard errors for the comparison of Temp:Gender terms depending if the comparison
# is between Temp:Gender combinations within or outside the same Subject

diff.Temp.Gender <- lmerTest::difflsmeans(breath.fit.lmer, test.eff="Temp:Gender")
#print(diff.Temp.Gender) # LOTS of output -- too much to print here
plot(diff.Temp.Gender)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed



##---part331b;
# The lsmeans() package also does these types of comparisons
# Now it computes the approximate df using the KR approximation - hurrah
# You have to be careful to detach some of the earlier packages because of name conflicts
# There is no compact letter display though and no plotting method for its output - bummer

names(sessionInfo()$otherPkgs) 
detach(package:lmerTest) # go back to regular lmer
library(lme4)

# We need to refit WITHOUT lmerTest so that the class of the package is correct
breath.fit.lmer <- lme4::lmer(Time ~ Temp + Gender + Temp:Gender +
     (1 | Subject),
     data=breath_nooutlier)

summary(breath.fit.lmer)


library(lsmeans) 
diff.Gender2 <- lsmeans::lsmeans(breath.fit.lmer,   list(pairwise~Gender),   adjust="tukey")
print(diff.Gender2)
lsmeans::lsmeans(breath.fit.lmer,   list(pairwise~Temp:Gender), adjust="tukey")
##---part331e;
sink()


sink('breath-R-331-temp.txt', split=TRUE)
diff.Temp2 <- lsmeans::lsmeans(breath.fit.lmer,   list(pairwise~Temp), adjust="tukey")
print(diff.Temp2)
sink()


# Multiple comparisons using MULTCOMP

# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!

library(multcomp)
cat("*** Multiple comparison for Genders *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE) )
summary(Gender.mc) 
confint(Gender.mc)

sink('breath-R-332.txt', split=TRUE)
##---part332b;
cat("*** Multiple comparison for Genders ***\n",
    "    but with correct degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE), 
                 df=round(diff.Gender2$"Gender pairwise differences"$df[1]))
summary(Gender.mc) 
confint(Gender.mc)
plot(Gender.mc)

# compact letter display (the joined line plots)
Gender.mc.cld <- cld(Gender.mc)
Gender.mc.cld
##---part332e;
sink()

sink('breath-R-332-Temp.txt', split=TRUE)
Temp.mc <- glht(breath.fit.lmer, linfct=mcp(Temp="Tukey",interaction_average=TRUE),
                df=round(diff.Temp2$"Temp pairwise differences"$df[1]))
cat("*** Multiple comparions for Temp ***\n",
    "    with correct df \n\n")
summary(Temp.mc)
confint(Temp.mc)
plot(Temp.mc)

Temp.mc.cld <- cld(Temp.mc)
Temp.mc.cld
sink()


# In order to get the MCP on the Temp:Gender combinations, we need to refit
# The model using a pseudofactor
breath$TF <- interaction(breath$Temp, breath$Gender)
breath.fit.lmer2 <- lmer(Time ~ -1 + TF + (1 | Subject),
     data=breath_nooutlier)
anova(breath.fit.lmer2, data=breath_nooutlier)
summary(breath.fit.lmer2)


# Note the different standard errors for the comparison depending if the comparison
# is between Temp:Gender combinations within or outside the same site.
# There is no easy way to determine the df for some of the comparisons and they differ depending
# if the comparison is inter or intra site. To be safe, we will use the smaller df corresponding
# to inter-site comparisons () which needs to be done by hand (or look at the SAS output)

Temp.Gender.mc <- glht(breath.fit.lmer2, linfct=mcp(TF="Tukey"), 
                       df=round(diff.Gender2$"Gender pairwise differences"$df[1]))

cat("*** Multiple comparisons for Temp:Gender ***\n",
    "    The df are not easily determined as they vary if these are\n",
    "    intra- or inter-Subject comparisons. Use a conservative value\n\n")
summary(Temp.Gender.mc)
confint(Temp.Gender.mc)
plot(Temp.Gender.mc)

Temp.Gender.mc.cld <- cld(Temp.Gender.mc)
Temp.Gender.mc.cld





# Extract the variance components
vc <- VarCorr(breath.fit.lmer)
as.matrix(vc)

# Or look at the slots directly
slotNames(summary(breath.fit.lmer))
data.frame(summary(breath.fit.lmer)@REmat)





# diagnostic plots.
# get the residual plot and normal probability plot

png('breath-R-500-diagnostics.png')
layout(matrix(1:4, 2,2))
qqnorm(resid(breath.fit.lmer),main="Q-Q plot for residuals")

plot(resid(breath.fit.lmer) ~ fitted(breath.fit.lmer),main="residual plot")
abline(h=0)

qqnorm(ranef(breath.fit.lmer)$Subject$"(Intercept)", main="Q-Q plot for the random Subject effect" )
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed
dev.off()


#---------------------------- Now to adjust for body height (which serves a surrogate for body size) -----
###-------------------------- Using lmer() function  ------------------

names(sessionInfo()$otherPkgs) 
detach(package:nlme)  # it interferes with lmerTest
detach(package:lsmeans) # it interferes also with lmerTest
library(lmerTest)  # computed p-values for lmer() calls - hurrah


# Check that all relevant variables are factors
str(breath)

##---part300-heightb;
breath.fit.height.lmerTest <- lmerTest::lmer(Time ~ Temp + Gender + Temp:Gender + Height+
     (1 | Subject),
     data=breath_nooutlier)
##---part300-heighte;


sink('breath-R-310-height.txt', split=TRUE)
##---part310b;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(breath.fit.height.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(breath.fit.height.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(breath.fit.height.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##---part310-heighte;
sink()

# Summary table
summary(breath.fit.height.lmerTest)

# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff='Temp')
lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff='Gender')
GT.means.ri <- lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff='Temp:Gender') # LOTS of output!

plot(lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff="Temp"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff="Gender"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

plot(lmerTest::lsmeans(breath.fit.height.lmerTest, test.eff="Temp:Gender"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed



png("breath-R-370-height.png")
##---part370-heightb;
# Create plot of the fitted GT values along with confidence limits for each
plot(as.numeric(as.character(GT.means.ht$lsmeans.table$Temp))+.5*(GT.means.ht$lsmeans.table$Gender=="Female"),
                GT.means.ht$lsmeans.table$Estimate,
                ylim=range(GT.means.ht$lsmeans.table[,c("Lower CI","Upper CI")]),
                ylab="Predicted Breath Holding Time",
                xlab='Temperature',
                main="Predicted breath holding times with 95% ci")
invisible(lapply(split(GT.means.ht$lsmeans.table, GT.means.ht$lsmeans.table$Gender),
          function(x){
            lines(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"),x$Estimate, lty=1+(x$Gender=="Female")[1])
            segments(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Lower CI",
                     as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Upper CI")
          }))
##---part370-heighte;
dev.off()



# Estimate the difference in the lsmeans. Notice that NO adjustment has been made
# for multiple comparisions, i.e. no Tukey adjustment
diff.Gender <- lmerTest::difflsmeans(breath.fit.height.lmerTest, test.eff="Gender")
print(diff.Gender)
plot(diff.Gender)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed



diff.Temp <- lmerTest::difflsmeans(breath.fit.height.lmer, test.eff="Temp")
print(diff.Temp)
plot(diff.Temp)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed


# Note the different standard errors for the comparison of Temp:Gender terms depending if the comparison
# is between Temp:Gender combinations within or outside the same Subject

diff.Temp.Gender <- lmerTest::difflsmeans(breath.fit.height.lmer, test.eff="Temp:Gender")
#print(diff.Temp.Gender) # LOTS of output -- too much to print here
plot(diff.Temp.Gender)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed




# The lsmeans() package also does these types of comparisons
# Now it computes the approximate df using the KR approximation - hurrah
# You have to be careful to detach some of the earlier packages because of name conflicts
# There is no compact letter display though and no plotting method for its output - bummer

names(sessionInfo()$otherPkgs) 
detach(package:lmerTest) # go back to regular lmer
library(lme4)

# We need to refit WITHOUT lmerTest so that the class of the package is correct
breath.fit.height.lmer <- lme4::lmer(Time ~ Temp + Gender + Temp:Gender +
     (1 | Subject),
     data=breath_nooutlier)

summary(breath.fit.height.lmer)


library(lsmeans) 
diff.Gender2 <- lsmeans::lsmeans(breath.fit.height.lmer,   list(pairwise~Gender),   adjust="tukey")
print(diff.Gender2)
lsmeans::lsmeans(breath.fit.height.lmer,   list(pairwise~Temp:Gender), adjust="tukey")



diff.Temp2 <- lsmeans::lsmeans(breath.fit.height.lmer,   list(pairwise~Temp), adjust="tukey")
print(diff.Temp2)


# Multiple comparisons using MULTCOMP

# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!

library(multcomp)
cat("*** Multiple comparison for Genders *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.height.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE) )
summary(Gender.mc) 
confint(Gender.mc)


cat("*** Multiple comparison for Genders ***\n",
    "    but with correct degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.height.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE), 
                 df=round(diff.Gender2$"Gender pairwise differences"$df[1]))
summary(Gender.mc) 
confint(Gender.mc)
plot(Gender.mc)

# compact letter display (the joined line plots)
Gender.mc.cld <- cld(Gender.mc)
Gender.mc.cld



Temp.mc <- glht(breath.fit.height.lmer, linfct=mcp(Temp="Tukey",interaction_average=TRUE),
                df=round(diff.Temp2$"Temp pairwise differences"$df[1]))
cat("*** Multiple comparions for Temp ***\n",
    "    with correct df \n\n")
summary(Temp.mc)
confint(Temp.mc)
plot(Temp.mc)

Temp.mc.cld <- cld(Temp.mc)
Temp.mc.cld



# In order to get the MCP on the Temp:Gender combinations, we need to refit
# The model using a pseudofactor
breath$TF <- interaction(breath$Temp, breath$Gender)
breath.fit.height.lmer2 <- lmer(Time ~ -1 + TF + (1 | Subject),
     data=breath_nooutlier)
anova(breath.fit.height.lmer2, data=breath_nooutlier)
summary(breath.fit.height.lmer2)


# Note the different standard errors for the comparison depending if the comparison
# is between Temp:Gender combinations within or outside the same site.
# There is no easy way to determine the df for some of the comparisons and they differ depending
# if the comparison is inter or intra site. To be safe, we will use the smaller df corresponding
# to inter-site comparisons () which needs to be done by hand (or look at the SAS output)

Temp.Gender.mc <- glht(breath.fit.height.lmer2, linfct=mcp(TF="Tukey"), 
                       df=round(diff.Gender2$"Gender pairwise differences"$df[1]))

cat("*** Multiple comparisons for Temp:Gender ***\n",
    "    The df are not easily determined as they vary if these are\n",
    "    intra- or inter-Subject comparisons. Use a conservative value\n\n")
summary(Temp.Gender.mc)
confint(Temp.Gender.mc)
plot(Temp.Gender.mc)

Temp.Gender.mc.cld <- cld(Temp.Gender.mc)
Temp.Gender.mc.cld





# Extract the variance components
vc <- VarCorr(breath.fit.height.lmer)
as.matrix(vc)

# Or look at the slots directly
slotNames(summary(breath.fit.height.lmer))
data.frame(summary(breath.fit.height.lmer)@REmat)





# diagnostic plots.
# get the residual plot and normal probability plot


layout(matrix(1:4, 2,2))
qqnorm(resid(breath.fit.height.lmer),main="Q-Q plot for residuals")

plot(resid(breath.fit.height.lmer) ~ fitted(breath.fit.height.lmer),main="residual plot")
abline(h=0)

qqnorm(ranef(breath.fit.height.lmer)$Subject$"(Intercept)", main="Q-Q plot for the random Subject effect" )
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed


#####################################################################################################
#---------------------------- Random intercept model-----
###-------------------------- Using lmer() function  ------------------

names(sessionInfo()$otherPkgs) 
detach(package:nlme)  # it interferes with lmerTest
detach(package:lsmeans) # it interferes also with lmerTest
library(lmerTest)  # computed p-values for lmer() calls - hurrah


# Check that all relevant variables are factors
str(breath_nooutlier)

# Convert the temperature variable from a factor to a continuous variable
breath_nooutlier2 <- breath_nooutlier
breath_nooutlier2$Temp <- as.numeric(as.character(breath_nooutlier2$Temp))
str(breath_nooutlier2)


##---part300-rib;
breath.fit.ri.lmerTest <- lmerTest::lmer(Time ~ Temp + Gender + Temp:Gender +
     (1 | Subject),
     data=breath_nooutlier2)
##---part300-rie;


sink('breath-R-310-ri.txt', split=TRUE)
##---part310-rib;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(breath.fit.ri.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(breath.fit.ri.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(breath.fit.ri.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##---part310-rie;
sink()

# Summary table
summary(breath.fit.ri.lmerTest)

# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!f
# Because temperature is continuous, a simple lsmean of temperature makes no sense.
lmerTest::lsmeans(breath.fit.ri.lmerTest, test.eff='Gender')

plot(lmerTest::lsmeans(breath.fit.ri.lmerTest, test.eff="Gender"))
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed

# Estimate the difference in the lsmeans. Notice that NO adjustment has been made
# for multiple comparisions, i.e. no Tukey adjustment
# Again, because Temperature is continuous, it makes no sense to compute lsmeans for Temp
diff.Gender <- lmerTest::difflsmeans(breath.fit.ri.lmerTest, test.eff="Gender")
print(diff.Gender)
plot(diff.Gender)
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed




# The lsmeans() package also does these types of comparisons
# Now it computes the approximate df using the KR approximation - hurrah
# You have to be careful to detach some of the earlier packages because of name conflicts
# There is no compact letter display though and no plotting method for its output - bummer

names(sessionInfo()$otherPkgs) 
detach(package:lmerTest) # go back to regular lmer
library(lme4)

# We need to refit WITHOUT lmerTest so that the class of the package is correct
breath.fit.ri.lmer <- lme4::lmer(Time ~ Temp + Gender + Temp:Gender +
     (1 | Subject),
     data=breath_nooutlier2)

summary(breath.fit.ri.lmer)


library(lsmeans) 
diff.Gender2 <- lsmeans::lsmeans(breath.fit.ri.lmer,   list(pairwise~Gender),   adjust="tukey")
print(diff.Gender2)




# Multiple comparisons using MULTCOMP

# Multiple comparisons for each of the main effects and the interactions.
# Notice that you need to "average" over interactions when comparing main effects
# and that the default method is to use infinite degrees of freedom.
# If you know the df for the contrast, you should specify it.
# This may be difficult to determine in advance and varies depending on the effect being tested
# so BE CAREFUL!
# It makes no sense to do a multiple comparison on temperature since it is a continuous variable.

library(multcomp)
cat("*** Multiple comparison for Genders *** \n",
    "    but with infinite degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.ri.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE) )
summary(Gender.mc) 
confint(Gender.mc)


cat("*** Multiple comparison for Genders ***\n",
    "    but with correct degrees of freedom *** \n\n")
Gender.mc <-glht(breath.fit.ri.lmer, linfct=mcp(Gender="Tukey", interaction_average=TRUE), 
                 df=round(diff.Gender2$"Gender pairwise differences"$df[1]))
summary(Gender.mc) 
confint(Gender.mc)
plot(Gender.mc)

# compact letter display (the joined line plots)
Gender.mc.cld <- cld(Gender.mc)
Gender.mc.cld




# Extract the variance components
vc <- VarCorr(breath.fit.ri.lmer)
as.matrix(vc)

# Or look at the slots directly
slotNames(summary(breath.fit.ri.lmer))
data.frame(summary(breath.fit.ri.lmer)@REmat)


# Make a plot of predicted values a each gender/temperature comparison
# We find the lsmean for each gender at the various temperature levels
##---part370-rib;
GT.means.ri <- do.call(rbind, lapply( unique(breath_nooutlier2$Temp), function(x){
   cbind(Temp=x,lsmeans::lsmeans(breath.fit.ri.lmer, ~Gender, at=list(Temp=x))[[1]])
   }))
GT.means.ri

png("breath-R-370-ri.png")
# Create plot of the fitted GT values along with confidence limits for each
plot(as.numeric(as.character(GT.means.ri$Temp))+.5*(GT.means.ri$Gender=="Female"),
                GT.means.ri$lsmean,
                ylim=range(GT.means.ri[,c("lower.CL","upper.CL")]),
                ylab="Predicted Breath Holding Time",
                xlab='Temperature',
                main="Predicted breath holding times with 95% ci under RI model")
invisible(lapply(split(GT.means.ri, GT.means.ri$Gender),
          function(x){
            lines(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"),x$lsmean, lty=1+(x$Gender=="Female")[1])
            segments(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"lower.CL",
                     as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"upper.CL")
          }))
##---part370-rie;
dev.off()



# diagnostic plots.
# get the residual plot and normal probability plot


layout(matrix(1:4, 2,2))
qqnorm(resid(breath.fit.ri.lmer),main="Q-Q plot for residuals")

plot(resid(breath.fit.ri.lmer) ~ fitted(breath.fit.ri.lmer),main="residual plot")
abline(h=0)

qqnorm(ranef(breath.fit.ri.lmer)$Subject$"(Intercept)", main="Q-Q plot for the random Subject effect" )
par(mfrow = c(1, 1))  # force the previous plot to show; don't know why needed










