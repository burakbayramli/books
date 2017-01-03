# Bioremidation of old gasoline stations.
# Split-plot in time; main plots in CRD

# 2015-07-18 CJS ggplot; split; lmerTest
# 2013-03-07 CJS First Edition

# After gasoline stations are decomissioned, there is often residual 
# contamination by organic solvents.
# Can natural fungi be used to reduce the contaminant load?
# Ten different locations on a former gas station were selected. These were 
# randomly assigned to different treatments by different fungi. The solvent 
# concentration was measured for several weeks.

# Lines starting with ##***part001b; or ##***part001e; bracket the source
# line for inclusion by LaTex and usually are not coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected

library(ggplot2)
library(lmerTest)
library(lsmeans)
library(nlme)
library(plyr)

source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")


# Read in the data
##***part000b;
sink('fungus-R-000.txt', split=TRUE)
solvent <- read.csv('fungus.csv', header=TRUE)
cat('*** Part of the raw data *** \n\n')
# Don't forget to change to factor variables
solvent$Site   <- as.factor(solvent$Site)
solvent$Fungus <- as.factor(solvent$Fungus)
solvent$Time   <- as.factor(solvent$Time)
solvent$Trt    <- with(solvent, interaction(Fungus,Time))
solvent[1:5,]
##***part000e;
sink()


##***part001b;
# Get side by side dot plots for each treatment
prelimplot <- ggplot(data=solvent, aes(x=Trt, y=Solvent))+
  ggtitle("Side-by-Side dot/box plot")+
  geom_point(size=4, position=position_jitter(w=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2)
prelimplot
##***part001e;
ggsave(plot=prelimplot, file="fungus-R-001.png", h=4, w=6, units="in", dpi=300 )



# Compute some summary statistics for each group
sink('fungus-R-002.txt', split=TRUE)
##***part002b;
# We don't compute standard errors because design is NOT a CRD
report<- ddply(solvent, c("Time","Fungus"), summarize,
               n=length(Solvent),
               mean=mean(Solvent),
               sd  =sd  (Solvent))
cat("*** Summary of raw means and standard deviations *** \n\n")
report
##***part002e;
sink()


# Plot the std deviation vs the mean to see a transformation is needed
sdvsmean <- ggplot(data=report, aes(x=log(mean), y=log(sd)))+
   ggtitle("log(SD) vs log(mean)")+
   geom_point()
sdvsmean
# There may be one combination of Fungus+Time where the SD appears to be about 1/2 of the rest.
# This requires further investigation.


##***part003b;
# Create the profile plot
profileplot <- ggplot(data=report, aes(x=Time, y=mean, group=Fungus, linetype=Fungus, color=Fungus))+
   ggtitle("Profile plot")+
   geom_line(size=2)
profileplot
##***part003e;
ggsave(plot=profileplot, file="fungus-R-003.png",  h=4, w=6, units="in", dpi=300)





#---------------------------------------------------------------------------------------

# Classical analysis using aov() function
sink('fungus-R-010.txt', split=TRUE)
##***part010b;
# check that all relevant variables are factors
str(solvent)
solvent.fit.aov <- aov( Solvent ~ Fungus * Time + Error(Site), data = solvent)
cat("*** Results from classic aov() fit to the design ***\n\n")
summary(solvent.fit.aov)
##***part010e;
sink()


#----------------------------------------------------------------------------------------------
# Now for the analysis using lme in the nlme package

##***part020b;
library(nlme)
# Check that all relevant variables are factors
str(solvent)
solvent.fit <- lme(fixed=Solvent ~ Time + Fungus + Time:Fungus,
     random=~ 1 | Site,
     data=solvent)
##***part0203;

sink('fungus-R-021.txt', split=TRUE)
##***part021b;     
# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# for details.
cat("*** Tests for fixed effects from lme(). ***\n",
    "    These are conditional on the variance component estimates ***\n\n")
anova(solvent.fit)
##***part021e;
sink()

sink('fungus-R-022.txt', split=TRUE)
##***part022b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
summary(solvent.fit)
nlme::VarCorr(solvent.fit)  # Need to override lmer() VarCOrr
##***part022e;
sink()

sink('fungus-R-023.txt', split=TRUE)
##***part023b;
# Get the lsmeans for each level 
solvent.fit.lme.f.lsmo <- lsmeans::lsmeans(solvent.fit,   ~Fungus)
solvent.fit.lme.t.lsmo <- lsmeans::lsmeans(solvent.fit,   ~Time)
solvent.fit.lme.tf.lsmo<- lsmeans::lsmeans(solvent.fit,   ~Time:Fungus)
summary(solvent.fit.lme.f.lsmo)
summary(solvent.fit.lme.t.lsmo)
summary(solvent.fit.lme.tf.lsmo)
sink()



sink('fungus-R-025.txt', split=TRUE)
##***part025b;
cat("*** Multiple comparison for Fungus ***\n")
summary(pairs(solvent.fit.lme.f.lsmo), infer=TRUE)

solvent.fit.lme.f.cld  <- cld(solvent.fit.lme.f.lsmo)
solvent.fit.lme.f.cld  

solvent.fit.lme.f.cldplot <- sf.cld.plot.line(solvent.fit.lme.f.cld, "Fungus")
solvent.fit.lme.f.cldplot
##***part025e;
sink()
ggsave(plot=solvent.fit.lme.f.cldplot, file="fungus-R-025.png", h=4, w=6, units="in", dpi=300)


sink('fungus-R-026.txt', split=TRUE)
##***part026b;
cat("*** Multiple comparison for Time ***\n")
summary(pairs(solvent.fit.lme.t.lsmo), infer=TRUE)

solvent.fit.lme.t.cld  <- cld(solvent.fit.lme.t.lsmo)
solvent.fit.lme.t.cld 

solvent.fit.lme.t.cldplot <- sf.cld.plot.line(solvent.fit.lme.t.cld, "Time")
solvent.fit.lme.t.cldplot
##***part026e;
sink()
ggsave(plot=solvent.fit.lme.t.cldplot, file="fungus-R-026.png", h=4, w=6, units="in", dpi=300)




sink('fungus-R-027b.txt', split=TRUE)
##***part027bb;
# Note the different standard errors for the comparison depending if the comparison
# is between Time:Fungus combinations within or outside the same Site.
cat("*** Multiple comparisons for Time:Fungus ***\n")
summary(pairs(solvent.fit.lme.tf.lsmo), infer=TRUE)

solvent.fit.lme.tf.cld  <- cld(solvent.fit.lme.tf.lsmo)
solvent.fit.lme.tf.cld 

# we need to create a treatment variable
solvent.fit.lme.tf.cld$trt <- paste(solvent.fit.lme.tf.cld$Fungus, '.', solvent.fit.lme.tf.cld$Time, sep="") 
solvent.fit.lme.tf.cldplot <- sf.cld.plot.line(solvent.fit.lme.tf.cld, "trt")
solvent.fit.lme.tf.cldplot
##***part027be;
sink()
ggsave(plot=solvent.fit.lme.tf.cldplot, file="fungus-R-027b.png", h=4, w=6, units="in", dpi=300)


png('fungus-R-028.png')
##***part028b;
# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(solvent.fit)
plot2 <- plot(solvent.fit, main="Residual plot")
plot3 <- plot(solvent.fit, Solvent ~ fitted(.), abline=c(0,1),
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
str(solvent)

##***part300b;
solvent.fit.lmer <- lmerTest::lmer(Solvent ~ Time + Fungus + Time:Fungus +
     (1 | Site),
     data=solvent)
##***part300e;


sink('fungus-R-310.txt', split=TRUE)
##***part310b;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(solvent.fit.lmer)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(solvent.fit.lmer, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(solvent.fit.lmer, ddf="lme4")        # the table from the original lmer() without p-values.
##***part310e;
sink()

# Summary table
summary(solvent.fit.lmer)

# Multiple comparison and cld for fungus treatments
sink('fungus-R-320.txt', split=TRUE)
##***part320b;
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
cat("*** Multiple means and CLD display for Fungus ***\n")
solvent.fit.lmer.f.lsmo <- lsmeans::lsmeans(solvent.fit.lmer,   ~Fungus)
solvent.fit.lmer.f.cld  <- cld(solvent.fit.lmer.f.lsmo)
solvent.fit.lmer.f.cld 
# plot the marginal means and the cld responses
solvent.fit.lmer.f.cldplot <- sf.cld.plot.line(solvent.fit.lmer.f.cld, "Fungus")
solvent.fit.lmer.f.cldplot
##***part320e;
sink()

sink('fungus-R-330.txt', split=TRUE)
##***part330b;
cat("*** Multiple comparison for Fungus - pairwise differences ***\n")
solvent.fit.lmer.f.pairs <- summary(pairs(solvent.fit.lme.f.lsmo), infer=TRUE)
solvent.fit.lmer.f.pairs
##***part330e;
sink()


# similar code can be used to compare the effects of time (but not very interesting)
# or for interactions (again not interesting)

solvent.fit.lmer.tf.lsmo<- lsmeans::lsmeans(solvent.fit.lmer,   ~Time:Fungus)
summary(solvent.fit.lmer.tf.lsmo)


cat("*** Multiple comparison and CLD display for Time ***\n")
solvent.fit.lmer.t.lsmo <- lsmeans::lsmeans(solvent.fit.lmer,   ~Time)

solvent.fit.lmer.t.cld  <- cld(solvent.fit.lmer.t.lsmo)
solvent.fit.lmer.t.cld 

solvent.fit.lmer.t.cldplot <- sf.cld.plot.line(solvent.fit.lmer.t.cld, "Time")
solvent.fit.lmer.t.cldplot

summary(pairs(solvent.fit.lmer.t.lsmo), infer=TRUE)



cat("*** Multiple comparisons for Time:Fungus ***\n")
# Note the different standard errors for the comparison depending if the comparison
# is between Time:Fungus combinations within or outside the same Site.
solvent.fit.lmer.tf.lsmo<- lsmeans::lsmeans(solvent.fit.lmer,   ~Time:Fungus)

solvent.fit.lmer.tf.cld  <- cld(solvent.fit.lmer.tf.lsmo)
solvent.fit.lmer.tf.cld 

# we need to create a treatment variable before plotting the cld vales
solvent.fit.lmer.tf.cld$trt <- paste(solvent.fit.lmer.tf.cld$Fungus, '.', solvent.fit.lmer.tf.cld$Time, sep="") 
solvent.fit.lmer.tf.cldplot <- sf.cld.plot.line(solvent.fit.lmer.tf.cld, "trt")
solvent.fit.lmer.tf.cldplot

summary(pairs(solvent.fit.lme.tf.lsmo), infer=TRUE)




sink("fungus-R-400.txt", split=TRUE)
##***part400b;
# Extract the variance components
vc <- VarCorr(solvent.fit.lmer)
as.matrix(vc)
##***part400e;
sink()




# diagnostic plots.
# get the residual plot and normal probability plot
# Sigh... in 2015-07, the authors of the gridExtra changed the functionality so now the revised code
# needs to be used.
diagplot <- sf.autoplot.lmer(solvent.fit.lmer)
grid.newpage()
grid.draw(diagplot)

