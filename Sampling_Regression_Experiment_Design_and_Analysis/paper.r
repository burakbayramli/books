# Paper strength.
# Split-Plot design with main plots in CRD

# 2014-08-12 CJS remove base R graphics; revised lsmeans usage; schwarz functions
# 2014-03-14 CJS ggplot; revised lme4, lmerTest, lsmeans commands
# 2013-03-07 CJS First Edition


# Paper strengh was measured in an experiment where different batches of
# pulp were selected and randomly assigned to 3 different methods of
# pulping. Each batch was then split into parts and each part was cooked at
# different temperatures. Finally the strength of the paper was measured.

# Lines starting with ##***part001b; or ##***part001e; bracket the source
# line for inclusion by LaTex and usually are not coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected
options(width=200)

# which libraries are needed
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(lsmeans)
library(nlme)
library(plyr)

# get the schwarz functions for summaries etc
source("../../schwarz.functions.r")


# Read in the data
##***part-readdatab;
sink('paper-R-readdata.txt', split=TRUE)
strength <- read.csv('paper.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
cat('*** Part of the raw data *** \n\n')
# Don't forget to change method and temp to factors
strength$Method <- as.factor(strength$Method)
strength$Temp   <- as.factor(strength$Temp)
strength$Trt    <- with(strength, interaction(Method,Temp))
strength[1:5,]
##***part-readdatae;
sink()



# Get side by side dot plots for each treatment
##***part-dotplotb;
plot.dotplot <- ggplot(data=strength, aes(x=Trt, y=Strength))+
    ggtitle("Dot plots to check for outliers")+
    xlab("Treatment (Method.Temperature)")+ylab("Paper strength")+
    geom_point(position=position_jitter(height=0.2, width=0.2), size=2)+
    geom_boxplot(alpha=0.1)
plot.dotplot
##***part-dotplote;

ggsave(plot=plot.dotplot, file="paper-R-dotplot.png", height=4, width=6, units="in")



# Make a nice report showing summary statistics by treatment in using the plyr()
# package as implemented in my summary function 
# Compute some summary statistics for each group
sink('paper-R-sumstat.txt', split=TRUE)
##***part-sumstatb;
cat("*** Summary of raw means and standard deviations *** \n\n")
report <- ddply(strength, c("Temp","Method"),  
                sf.simple.summary, 
                variable="Strength")
report
##***part-sumstate;
sink()




# check to see if the sd increases with the mean
ggplot(data=report, aes(x=log(mean),y=log(sd)))+
   ggtitle("Does the SD increase with the mean?")+
   geom_point()



# The interaction plot
# We don't compute the se or confidence intervals for each
# point because of the split-plot structure makes it difficult
# to do so from the raw data.
##***part-interaction-plotb;
plot.interaction <- ggplot(data=report, aes(x=Temp, y=mean, 
                  group=Method, color=Method, linetype=Method, shape=Method ))+
  ggtitle("Interaction plot based on raw data")+
  xlab("Temperature")+ ylab("Mean Strength")+
  geom_point(size=3)+
  geom_line()
plot.interaction
##***part-interaction-plote;
ggsave(plot=plot.interaction, file="paper-R-interaction-plot.png", height=4, width=6, units="in")





######################################################################################
# Classical analysis using aov() function
sink('paper-R-010.txt', split=TRUE)
##***part010b;
# check that all relevant variables are factors
str(strength)
strength.fit.aov <- aov( Strength ~ Method * Temp + Error(Batch), data = strength)
cat("*** Results from classic aov() fit to the design ***\n\n")
summary(strength.fit.aov)
##***part010e;
sink()

# The problem with the aov() function is that it is difficult to get estimates
# of lsmeans or pairwise comparisons etc.





#########################################################################################
# Now for the analysis using lme in the nlme package

##***part020b;
library(nlme)
# Check that all relevant variables are factors
str(strength)
strength.fit <- lme(fixed=Strength ~ Temp + Method + Temp:Method,
     random=~ 1 | Batch,
     data=strength)
##***part020e;

sink('paper-R-021.txt', split=TRUE)
##***part021b;     
# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# for details.
cat("*** Tests for fixed effects from lme(). ***\n",
    "    These are conditional on the variance component estimates ***\n\n")
# We specify the type argument to get the marginal (Type III) tests.
# Because the design is balanced, the default type="sequential" (TYpe I)
# are the same, but this is generally NOT true. Always use the Type III tests.
anova(strength.fit, type="marginal")
##***part021e;
sink()

sink('paper-R-022.txt', split=TRUE)
##***part022b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
summary(strength.fit)
nlme::VarCorr(strength.fit)  # Need to override lmer() VarCOrr
##***part022e;
sink()

sink('paper-R-023.txt', split=TRUE)
##***part023b;
# Get the lsmeans for each level using the lsmeans package

# We load the lsmeans package. It does the lsmeans for lme objects
# but the df is NOT specified (groan) and you need to specify by hand
temp.lsmo   <- lsmeans(strength.fit,  ~Temp)
method.lsmo <- lsmeans(strength.fit,  ~Method)
tm.lsmo     <- lsmeans(strength.fit,  ~Temp:Method)

summary(temp.lsmo)
summary(method.lsmo)
summary(tm.lsmo)

# Now for the comparisons between all pairs of lsmeans
# Again notice that df are not automatically computed and you must specify by hand
# This is the df for denominators for F tests of main effects, but is
# difficult to specify for interaction terms.
# Notice that if you use the same functions with lmer() it will compute
# the df using the KR approximation.
pairs(temp.lsmo)
pairs(method.lsmo)
pairs(tm.lsmo)
##***part023e;
sink()



png('paper-R-028.png')
##***part028b;
# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(strength.fit)
plot2 <- plot(strength.fit, main="Residual plot")
plot3 <- plot(strength.fit, Strength ~ fitted(.), abline=c(0,1),
   main="Observed vs Predicted")
print(plot1, position=c(0,.5,.5, 1), more=TRUE)
print(plot2, position=c(0, 0,.5, .5), more=TRUE)
print(plot3, position=c(.5,0, 1, .5))
##***part028e;
dev.off()






###***----------------------- Using lmer() function  ------------------
# The lmer() function provides a much simple way to specify 
# models with random effect. HOWEVER, the ordinary lmer() function
# does NOT produce p-values.
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
#     for details on why LMER does NOT produce p-values.
# Also see http://glmm.wikidot.com/faq for further information.

# Fortunately, the lmerTest package has recently been produced which 
# adds p-values based on a Satterthaite approximation 
#


# Check that all relevant variables are factors
str(strength)

##***part-lmermodelb;
strength.fit.lmerTest <- lmerTest::lmer(Strength ~ Temp + Method + Temp:Method +
     (1 | Batch),
     data=strength)
##***part-lmermodele;


sink('paper-R-lmerFtests.txt', split=TRUE)
##***part-lmerFtestsb;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(strength.fit.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(strength.fit.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(strength.fit.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##***part-lmerFtestse;
sink()


# Summary table
summary(strength.fit.lmerTest)



# Now get the lsmeans for the Temperature effect and construct the various plots.
sink('paper-R-lsmeans-temp.txt', split=TRUE)
##***part-lsmeans-tempb;
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
temp.lsmo   <- lsmeans::lsmeans(strength.fit.lmerTest, ~Temp)

# Find the CLD report with a tukey adjustments to get simultaneous confidence intervals and make a plot
temp.cld <- cld(temp.lsmo, adjust='tukey')
temp.cld

# get a cld bar plot
temp.cld.plot1 <- sf.cld.plot.bar(temp.cld, "Temp")+
     xlab("Temperature")+
     ylab("Mean paper strength and 95% ci")+
     ggtitle("Comparison of mean strength over temperatures with cld")
temp.cld.plot1

# get a cld line plot
temp.cld.plot2 <- sf.cld.plot.line(temp.cld, "Temp")+
  xlab("Temperature")+
  ylab("Mean paper strength and 95% ci")+
  ggtitle("Comparison of mean strength over temperatures with cld")
temp.cld.plot2

temp.pairs <- pairs(temp.lsmo, adjust='tukey')
summary(temp.pairs, infer=TRUE)
##***part-lsmeans-tempe;
sink()
ggsave(plot=temp.cld.plot1, file='paper-R-temp-cld-plot1.png', height=4, width=6, units="in")
ggsave(plot=temp.cld.plot2, file='paper-R-temp-cld-plot2.png', height=4, width=6, units="in")



# We repeat the above for the Methods factor
# Now get the lsmeans for the Temperature effect and construct the various plots.
sink('paper-R-lsmeans-method.txt', split=TRUE)
##***part-lsmeans-methodb;
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
method.lsmo   <- lsmeans::lsmeans(strength.fit.lmerTest, ~Method)

# Find the CLD report with a tukey adjustments to get simultaneous confidence intervals and make a plot
method.cld <- cld(method.lsmo, adjust='tukey')
method.cld

# get a cld bar plot
method.cld.plot1 <- sf.cld.plot.bar(method.cld, "Method")+
  xlab("Method")+
  ylab("Mean paper strength and 95% ci")+
  ggtitle("Comparison of mean strength over methods with cld")
method.cld.plot1

# get a cld line plot
method.cld.plot2 <- sf.cld.plot.line(method.cld, "Method")+
  xlab("Method")+
  ylab("Mean paper strength and 95% ci")+
  ggtitle("Comparison of mean strength over methods with cld")
method.cld.plot2

method.pairs <- pairs(method.lsmo, adjust='tukey')
summary(method.pairs, infer=TRUE)
##***part-lsmeans-methode;
sink()
ggsave(plot=method.cld.plot1, file='paper-R-method-cld-plot1.png', height=4, width=6, units="in")
ggsave(plot=method.cld.plot2, file='paper-R-method-cld-plot2.png', height=4, width=6, units="in")




# We repeat the above for the Temp:Methods interaction
# Now get the lsmeans for the Temperature effect and construct the various plots.
sink('paper-R-lsmeans-tm.txt', split=TRUE)
##***part-lsmeans-methodb;
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
tm.lsmo   <- lsmeans::lsmeans(strength.fit.lmerTest, ~Temp:Method)

# Find the CLD report with a tukey adjustments to get simultaneous confidence intervals and make a plot
tm.cld <- cld(tm.lsmo, adjust='tukey')
tm.cld

# get a cld bar plot but because there are two factors we need to add a new variable to the cld report
tm.cld$TM <- interaction(tm.cld$Temp, tm.cld$Method)
tm.cld.plot1 <- sf.cld.plot.bar(tm.cld, "TM")+
  xlab("Temp.Method")+
  ylab("Mean paper strength and 95% ci")+
  ggtitle("Comparison of mean strength over temp.methods with cld")
tm.cld.plot1

# get a cld line plot
tm.cld.plot2 <- sf.cld.plot.line(tm.cld, "TM")+
  xlab("Temp.Method")+
  ylab("Mean paper strength and 95% ci")+
  ggtitle("Comparison of mean strength over temp.methods with cld")
tm.cld.plot2

tm.pairs <- pairs(tm.lsmo, adjust='tukey')
summary(tm.pairs, infer=TRUE)
##***part-lsmeans-tme;
sink()
ggsave(plot=tm.cld.plot1, file='paper-R-tm-cld-plot1.png', height=4, width=6, units="in")
ggsave(plot=tm.cld.plot2, file='paper-R-tm-cld-plot2.png', height=4, width=6, units="in")




sink('paper-R-varcomp.txt', split=TRUE)
##***part-varcompb;
# Extract the variance components
vc <- VarCorr(strength.fit.lmerTest)
vc
##***part-varcompe;
sink()



# Diagnostic plots. 
##***part-diagplotb;
diag.qqplot <- ggplot(strength.fit.lmerTest, aes(sample=.resid)) + 
   stat_qq()+
   ggtitle("Q-Q plot for residuals")

diag.resplot <- ggplot(strength.fit.lmerTest, aes(x=.fitted, y=.resid))+
   ggtitle("Residual vs Predicted plot")+
   geom_point() + geom_hline(yintercept=0)

diag.qqplot.batch <- ggplot( data.frame(batch.eff=ranef(strength.fit.lmerTest)$Batch$"(Intercept)") , aes(sample=batch.eff))+
  stat_qq()+
  ggtitle("Q-Q plot for batch effect")

diag.plot <- arrangeGrob(diag.qqplot, diag.resplot, diag.qqplot.batch, nrow=2)
diag.plot
##***part-diagplote;
ggsave(plot=diag.plot, file='paper-R-diagplot.png', height=4, width=6, units="in")


