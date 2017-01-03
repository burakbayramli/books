# Pheromone effects upon wild type and anarchist colonies of bee}
# Split plot; Main plots in CRD; Multiple measurements at sub-plot level (pseudo-replication)

# 2015-07-25 CJS update for ggplot, split; ##*** etc
# 2013-03-07 CJS First Version

# This is based on an experiment by S. Hoover, Biological Sciences, Simon Fraser University.

# In normal honey-bee colonies,  the queen is the main reproductive bee in a 
# colony. Workers cannot mate, but they can lay unfertilized eggs, which 
# develop into males if reared.  Worker reproduction, while common in 
# queen-less colonies, is rare in queen-right colonies, despite the fact that 
# workers are more related  to their own sons than to those of the queen.

# In some colonies,  a rare behavioral syndrome, {\it anarchy} occurs, in 
# which substantial worker production of males occurs in queen-right 
# colonies. The level of worker reproduction in these anarchic colonies is 
# far greater than in a normal queen-right honey-bee colony.

# An experiment was conducted where different pheromones were applied to 
# groups of workers from normal (wild type) and anarchist colonies of bees.

# There were 4 anarchist colonies (all that existed at the time at SFU) and 
# 4 wild type colonies selected at random from all the wild type present at 
# SFU. A comb was removed from each colony, and incubated overnight.  About 
# 120 bees were taken from those that had emerged from the comb while it was
# in the incubator, and 30 were placed in cages, four separate cages from 
# each colony.  One of four types of pheromones were applied to each cage.

# After the bees emerged, the bees were scored on
# ovary development scores (a scale from 0-4). Between
# 20-30 bees (a few died) were scored from the 8x4=32 cages.


# Lines starting with ##***part001b; or ##***part001e; bracket the source
# line for inclusion by LaTex and usually are not coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected

source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(lsmeans)
library(nlme)
library(plyr)


# Read in the data
##***part000b;
sink('beeovary-R-000.txt', split=TRUE )
in.beeovary <- read.csv('beeovary.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
cat('*** Part of the raw data *** \n\n')
# Don't forget to change variables to factor
in.beeovary$Type        <- factor(in.beeovary$Type)
in.beeovary$Pheromone   <- factor(in.beeovary$Pheromone)
in.beeovary$Cage        <- factor(in.beeovary$Cage)
in.beeovary$Colony      <- factor(in.beeovary$Colony)
head(in.beeovary)
##***part000e;
sink()

sink("beeovary-R-000-avg.txt", split=TRUE)
##***part000-avgb;
# Reduce the pseudo-replicates to averages
beeovary <- ddply(in.beeovary,  c("Pheromone","Type","Colony","Cage"), summarize,
                  MeanScore=mean(Score),
                  NScore   =length(Score))
head(beeovary)
##***part000-avge;
sink()



# Get side by side dot plots for each treatment
beeovary$trt <- interaction(beeovary$Pheromone, beeovary$Type)
##***part001b;
prelim <- ggplot( data=beeovary, aes(x=trt, y=MeanScore))+
  ggtitle("MeanScore of Ovary Development")+
  xlab("Pheromone:Spray")+ylab("Mean Score by cage")+
  geom_point(position=position_jitter(w=0.2))+
  geom_boxplot(alpha=0.2, notch=TRUE)
prelim
##***part001e;
ggsave(plot=prelim, file="beeovary-R-001.png", h=4, w=6, units="in", dpi=300)  # send the plot to a png file


# Compute some summary statistics for each group
sink('beeovary-R-002.txt', split=TRUE)
##***part002b;
# We don't compute standard errors because design is NOT a CRD
report <- ddply(beeovary, c("Pheromone","Type"), summarize,
                Mean=mean(MeanScore),
                SD  =sd  (MeanScore),
                n   =length(MeanScore))
cat("*** Summary of raw means and standard deviations *** \n\n")
report
##***part002e;
sink()


# Plot the std deviation vs the mean to see a transformation is needed
ggplot(data=report, aes(x=log(Mean), y=log(SD)))+
         ggtitle("log(SD) vs log(Mean) to see if SD related to mean")+
         geom_point(size=4)
      

# Create the profile plot
##***part003b;
# Create the interaction plot.
interplot <- ggplot(data=report, aes(x=Type, y=Mean, group=Pheromone, color=Pheromone, linetype=Pheromone))+
  ggtitle("Profile plot")+
  geom_point(size=2, position=position_dodge(w=0.2))+
  geom_line(size=2, position=position_dodge(w=0.2))
interplot
##***part003e;
ggsave(plot=interplot, file="beeovary-R-003.png", h=4, w=6, units="in", dpi=300)




#----------------------------------------------------
# Classical analysis using aov() function
sink('beeovary-R-010.txt', split=TRUE)
##***part010b;
# check that all relevant variables are factors
str(beeovary)
beeovary.fit.aov <- aov( MeanScore ~ Type * Pheromone + Error(Colony), data = beeovary)
cat("*** Results from classic aov() fit to the design ***\n\n")
summary(beeovary.fit.aov)
##***part010e;
sink()


#-----------------------------------------------------------
# Now for the analysis using lme in the nlme package

##***part020b;
# Check that all relevant variables are factors
str(beeovary)
beeovary.fit <- nlme::lme(fixed=MeanScore ~ Pheromone + Type + Pheromone:Type,
     random=~ 1 | Colony,
     data=beeovary)
##***part0203;

sink('beeovary-R-021.txt', split=TRUE)
##***part021b;     
# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# for details.
cat("*** Tests for fixed effects from lme(). ***\n",
    "    These are conditional on the variance component estimates ***\n\n")
anova(beeovary.fit)
##***part021e;
sink()

sink('beeovary-R-022.txt', split=TRUE)
##***part022b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
summary(beeovary.fit)
nlme::VarCorr(beeovary.fit)  # Need to override lmer() VarCOrr
##***part022e;
sink()

sink('beeovary-R-023.txt', split=TRUE)
##***part023b;
# Get the lsmeans for each level 
beeovary.fit.type.lsmo <- lsmeans::lsmeans(beeovary.fit, ~Type)
beeovary.fit.pher.lsmo <- lsmeans::lsmeans(beeovary.fit, ~Pheromone)
beeovary.fit.pt.lsmo   <- lsmeans::lsmeans(beeovary.fit, ~Type:Pheromone)

summary(beeovary.fit.type.lsmo)
summary(beeovary.fit.pher.lsmo)
summary(beeovary.fit.pt.lsmo)

# Now for the comparisons between all pairs of lsmeans
# df determined by the containment method
summary(pairs(beeovary.fit.type.lsmo), infer=TRUE)
summary(pairs(beeovary.fit.pher.lsmo), infer=TRUE)
summary(pairs(beeovary.fit.pt.lsmo),   infer=TRUE)
##***part023e;
sink()



sink('beeovary-R-025.txt', split=TRUE)
##***part025b;
cat("*** Multiple comparison for Type ***\n")
beeovary.fit.type.cld <- cld(beeovary.fit.type.lsmo)
beeovary.fit.type.cld
beeovary.fit.type.plot <- sf.cld.plot.line(beeovary.fit.type.cld, "Type")+
  xlab('Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Types")
beeovary.fit.type.plot
##***part025e;
sink()
ggsave(plot=beeovary.fit.type.plot, file='beeovary-R-025.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-026.txt', split=TRUE)
##***part026b;
cat("*** Multiple comparison for Pheromone ***\n")
beeovary.fit.pher.cld <- cld(beeovary.fit.pher.lsmo)
beeovary.fit.pher.cld
beeovary.fit.pher.plot <- sf.cld.plot.line(beeovary.fit.pher.cld, "Pheromone")+
  xlab('Pheromone')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones")
beeovary.fit.pher.plot
##***part026e;
sink()
ggsave(plot=beeovary.fit.pher.plot, file='beeovary-R-026.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-027.txt', split=TRUE)
##***part027b;
cat("*** Multiple comparison for Pheromone:Type ***\n")
beeovary.fit.pt.cld <- cld(beeovary.fit.pt.lsmo)
beeovary.fit.pt.cld
beeovary.fit.pt.cld$trt <- interaction(beeovary.fit.pt.cld$Pheromone, beeovary.fit.pt.cld$Type, lex.order=FALSE)
beeovary.fit.pt.plot <- sf.cld.plot.line(beeovary.fit.pt.cld, "trt")+
  xlab('Pheromone:Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones:Type combinations")
beeovary.fit.pt.plot
##***part027e;
sink()
ggsave(plot=beeovary.fit.pt.plot, file='beeovary-R-027.png', h=4, w=6, unit="in", dpi=300)



png('beeovary-R-028.png')
##***part028b;
# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(beeovary.fit)
plot2 <- plot(beeovary.fit, main="Residual plot")
plot3 <- plot(beeovary.fit, MeanScore ~ fitted(.), abline=c(0,1),
   main="Observed vs Predicted")
print(plot1, position=c(0,.5,.5, 1), more=TRUE)
print(plot2, position=c(0, 0,.5, .5), more=TRUE)
print(plot3, position=c(.5,0, 1, .5))
##***part028e;
dev.off()
print(plot1, position=c(0,.5,.5, 1), more=TRUE)
print(plot2, position=c(0, 0,.5, .5), more=TRUE)
print(plot3, position=c(.5,0, 1, .5))



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
# You may nave to detach all packages and reload packages as there are name conflicts
# among the packages
#
# Check that all relevant variables are factors
str(beeovary)

##***part200b;
beeovary.fit.lmerTest <- lmerTest::lmer(MeanScore ~ Pheromone + Type + Pheromone:Type +
     (1 | Colony),
     data=beeovary)
##***part200e;

sink('beeovary-R-210.txt', split=TRUE)
##***part210b;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(beeovary.fit.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n",
    "Notice that the F-value is incorrect, but the p-value appear to be ok\n")
anova(beeovary.fit.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(beeovary.fit.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##***part210e;
sink()


# Summary table
summary(beeovary.fit.lmerTest)

sink('beeovary-R-222.txt', split=TRUE)
##***part222b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
VarCorr(beeovary.fit.lmerTest)  
##***part222e;
sink()



sink('beeovary-R-225.txt', split=TRUE)
##***part225b;
cat("*** Multiple comparison for Type ***\n")
beeovary.fit.lmerTest.type.lsmo <- lsmeans::lsmeans(beeovary.fit.lmerTest, ~Type)
summary(beeovary.fit.lmerTest.type.lsmo)
summary(pairs(beeovary.fit.lmerTest.type.lsmo), infer=TRUE)
beeovary.fit.lmerTest.type.cld <- cld(beeovary.fit.lmerTest.type.lsmo)
beeovary.fit.lmerTest.type.cld
beeovary.fit.lmerTest.type.plot <- sf.cld.plot.line(beeovary.fit.lmerTest.type.cld, "Type")+
  xlab('Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Types")
beeovary.fit.lmerTest.type.plot
##***part225e;
sink()
ggsave(plot=beeovary.fit.lmerTest.type.plot, file='beeovary-R-225.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-226.txt', split=TRUE)
##***part226b;
cat("*** Multiple comparison for Pheromone ***\n")
beeovary.fit.lmerTest.pher.lsmo <- lsmeans::lsmeans(beeovary.fit.lmerTest, ~Pheromone)
summary(beeovary.fit.lmerTest.pher.lsmo)
summary(pairs(beeovary.fit.lmerTest.pher.lsmo), infer=TRUE)

beeovary.fit.lmerTest.pher.cld <- cld(beeovary.fit.lmerTest.pher.lsmo)
beeovary.fit.lmerTest.pher.cld
beeovary.fit.lmerTest.pher.plot <- sf.cld.plot.line(beeovary.fit.lmerTest.pher.cld, "Pheromone")+
  xlab('Pheromone')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones")
beeovary.fit.lmerTest.pher.plot
##***part226e;
sink()
ggsave(plot=beeovary.fit.lmerTest.pher.plot, file='beeovary-R-226.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-227.txt', split=TRUE)
##***part227b;
cat("*** Multiple comparison for Pheromone:Type ***\n")
beeovary.fit.lmerTest.pt.lsmo   <- lsmeans::lsmeans(beeovary.fit.lmerTest, ~Type:Pheromone)
summary(beeovary.fit.lmerTest.pt.lsmo)

summary(pairs(beeovary.fit.lmerTest.pt.lsmo),   infer=TRUE)
 
beeovary.fit.lmerTest.pt.cld <- cld(beeovary.fit.lmerTest.pt.lsmo)
beeovary.fit.lmerTest.pt.cld
beeovary.fit.lmerTest.pt.cld$trt <- interaction(beeovary.fit.lmerTest.pt.cld$Pheromone, 
                                                beeovary.fit.lmerTest.pt.cld$Type, lex.order=FALSE)
beeovary.fit.lmerTest.pt.plot <- sf.cld.plot.line(beeovary.fit.lmerTest.pt.cld, "trt")+
  xlab('Pheromone:Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones:Type combinations")
beeovary.fit.lmerTest.pt.plot
##***part227e;
sink()
ggsave(plot=beeovary.fit.lmerTest.pt.plot, file='beeovary-R-227.png', h=4, w=6, unit="in", dpi=300)



png('beeovary-R-228.png')
##***part228b;
# diagnostic plots.
diagplot <- sf.autoplot.lmer(beeovary.fit.lmerTest)
grid.newpage()
grid.draw(diagplot)
##***part228e;
dev.off()
grid.newpage()
grid.draw(diagplot)


sink("beeovary-R-250.txt", split=TRUE)
##***part250b;
# Extract the variance components
vc <- VarCorr(beeovary.fit.lmerTest)
vc
##***part250e;
sink()









#-------------------------------------------------------------------------
# lmer() analysis on the raw scores
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
str(beeovary)

##***part300-indivb;
beeovary.fit.indiv.lmerTest <- lmerTest::lmer(Score ~ Pheromone + Type + Pheromone:Type +
                                (1 | Colony) + (1 | Cage),
                              data=in.beeovary)
##***part300-indive;

sink('beeovary-R-310-indiv.txt', split=TRUE)
##***part310-indivb;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Sattertrehwaite approximate\n")
anova(beeovary.fit.indiv.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n",
    "Notice that the F-value is incorrect, but the p-value appear to be ok")
anova(beeovary.fit.indiv.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(beeovary.fit.indiv.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.
##***part310-indive;
sink()


# Summary table
summary(beeovary.fit.indiv.lmerTest)

sink('beeovary-R-322.txt', split=TRUE)
##***part322b;
# Get the estimates of the variance components
cat("*** Estimates of variance components (and other stuff) ***\n\n")
VarCorr(beeovary.fit.indiv.lmerTest)  
##***part322e;
sink()

sink('beeovary-R-323.txt', split=TRUE)
##***part323b;
# Get the lsmeans for each level 
beeovary.fit.indiv.lmerTest.type.lsmo <- lsmeans::lsmeans(beeovary.fit.indiv.lmerTest, ~Type)
beeovary.fit.indiv.lmerTest.pher.lsmo <- lsmeans::lsmeans(beeovary.fit.indiv.lmerTest, ~Pheromone)
beeovary.fit.indiv.lmerTest.pt.lsmo   <- lsmeans::lsmeans(beeovary.fit.indiv.lmerTest, ~Type:Pheromone)

summary(beeovary.fit.indiv.lmerTest.type.lsmo)
summary(beeovary.fit.indiv.lmerTest.pher.lsmo)
summary(beeovary.fit.indiv.lmerTest.pt.lsmo)

# Now for the comparisons between all pairs of lsmeans
# df determined by the containment method
summary(pairs(beeovary.fit.indiv.lmerTest.type.lsmo), infer=TRUE)
summary(pairs(beeovary.fit.indiv.lmerTest.pher.lsmo), infer=TRUE)
summary(pairs(beeovary.fit.indiv.lmerTest.pt.lsmo),   infer=TRUE)
##***part323e;
sink()



sink('beeovary-R-325.txt', split=TRUE)
##***part325b;
cat("*** Multiple comparison for Type ***\n")
beeovary.fit.indiv.lmerTest.type.cld <- cld(beeovary.fit.indiv.lmerTest.type.lsmo)
beeovary.fit.indiv.lmerTest.type.cld
beeovary.fit.indiv.lmerTest.type.plot <- sf.cld.plot.line(beeovary.fit.indiv.lmerTest.type.cld, "Type")+
  xlab('Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Types")
beeovary.fit.indiv.lmerTest.type.plot
##***part325e;
sink()
ggsave(plot=beeovary.fit.indiv.lmerTest.type.plot, file='beeovary-R-325.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-326.txt', split=TRUE)
##***part326b;
cat("*** Multiple comparison for Pheromone ***\n")
beeovary.fit.indiv.lmerTest.pher.cld <- cld(beeovary.fit.indiv.lmerTest.pher.lsmo)
beeovary.fit.indiv.lmerTest.pher.cld
beeovary.fit.indiv.lmerTest.pher.plot <- sf.cld.plot.line(beeovary.fit.indiv.lmerTest.pher.cld, "Pheromone")+
  xlab('Pheromone')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones")
beeovary.fit.indiv.lmerTest.pher.plot
##***part326e;
sink()
ggsave(plot=beeovary.fit.indiv.lmerTest.pher.plot, file='beeovary-R-326.png', h=4, w=6, unit="in", dpi=300)


sink('beeovary-R-327.txt', split=TRUE)
##***part327b;
cat("*** Multiple comparison for Pheromone:Type ***\n")
beeovary.fit.indiv.lmerTest.pt.cld <- cld(beeovary.fit.indiv.lmerTest.pt.lsmo)
beeovary.fit.indiv.lmerTest.pt.cld
beeovary.fit.indiv.lmerTest.pt.cld$trt <- interaction(beeovary.fit.indiv.lmerTest.pt.cld$Pheromone, 
                                                beeovary.fit.indiv.lmerTest.pt.cld$Type, lex.order=FALSE)
beeovary.fit.indiv.lmerTest.pt.plot <- sf.cld.plot.line(beeovary.fit.indiv.lmerTest.pt.cld, "trt")+
  xlab('Pheromone:Type')+ylab("Mean Score")+
  ggtitle("Multiple comparison among Pheromones:Type combinations")
beeovary.fit.indiv.lmerTest.pt.plot
##***part327e;
sink()
ggsave(plot=beeovary.fit.indiv.lmerTest.pt.plot, file='beeovary-R-327.png', h=4, w=6, unit="in", dpi=300)



png('beeovary-R-328.png')
# diagnostic plots.
diagplot <- sf.autoplot.lmer(beeovary.fit.indiv.lmerTest)
grid.newpage()
grid.draw(diagplot)
dev.off()
grid.newpage()
grid.draw(diagplot)

