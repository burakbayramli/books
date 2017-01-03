# Effect of temperature and photoperiod on the gonadosomatic index.		
# 2015-07-10 CSJ update Type III using local contrast in lm rather than global setting
# 2014-10-30 CJS Update using ggplot, lsmeans 
# 2014-10-30 CJS Use slices on interaction terms

# A two factor experiment was conducted to investigate 
# the effect of water termperature			
# and the photoperiod on the gonadosomatic index of fish. 
# Separate tanks were randomized to the different treatment 
# combinations. A single measurement of all fish within the tank was
# obtained.

# Example of a two factor CRD analysis of variance using both balanced and unbalanced data

options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(ggplot2)
library(lsmeans)
library(plyr)
source("../../schwarz.functions.r") # basic summary and plotting functions

# Read in the actual data
sink("gsi-R-001.txt", split=TRUE)
##***part001b;
cat(" Effect of Temp and Photo on gsi levels in Fish \n\n")

gsi <- read.csv("gsi.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
# Define variables as factors as needed
gsi$TempF  <- factor(gsi$Temp)
gsi$PhotoF <- factor(gsi$Photo)
gsi$trtF   <- factor(interaction(gsi$Temp, gsi$Photo))

cat("Listing of part of the raw data \n")
gsi[1:10,]
##***part001e;
sink()

# Preliminary plot

##***part010b;
# Get side-by-side dot and box plots
plot1 <- ggplot(data=gsi, aes(x=trtF, y=gsi))+
  ggtitle("GSI levels in different Temp/Photo period combinations")+
  ylab("GSI")+xlab("Temp and Photo period")+
  geom_point( position=position_jitter(width=0.2), size=4)+
  geom_boxplot(alpha=0.2, width=0.5, notch=TRUE)
plot1
##***part010e;
ggsave(plot=plot1, file="gsi-R-010.png", height=4, width=6, units="in", dpi=300)


# Get some simple summary statistics
sink('gsi-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(gsi, c("PhotoF","TempF"),sf.simple.summary, variable="gsi", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part020e;
sink()

# Draw a profile plot
##***part030b;
profile.plot <-  ggplot(data=report, aes(x=PhotoF, y=mean, group=TempF, shape=TempF))+
  ggtitle("Profile plot of mean GSI scores")+
  ylab("Mean GSI with 95% ci")+xlab('Photo Period')+
  geom_point(size=4, position=position_dodge(.1))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.1, position=position_dodge(.1))+
  geom_line(position=position_dodge(.1), aes(linetype=TempF))
profile.plot
##***part030e;

ggsave(plot=profile.plot, file="gsi-R-030.png", height=4, width=6, unit="in", dpi=300)  # send the plot to a png file


# See if a transformation is needed
# Plot the log(std dev) vs. the log(mean) to see if transformation needed
#   under Taylor's Power Law 

taylor.plot <- ggplot(data=report, aes(x=log(mean), y=log(sd)))+
  ggtitle("Log(sd) vs log(mean) to see if transformation needed")+
  xlab("log(mean) of GSI")+ylab("log(sd) of GSI")+
  geom_point(size=4)+
  stat_smooth(method="lm", se=FALSE)
taylor.plot 

ggsave(plot=taylor.plot, file="gsi-R-taylorplot.png", height=4, width=6, units="in", dpi=300)  # send the plot to a png file




# Fit the linear model, get the anova table, and the usual stuff
# Because the design is BALANCED, R produces the correct sums of squares
# See the entry further down in this program
# about cautions with unbalanced data
sink('gsi-R-100.txt', split=TRUE)
##***part100b;
result.lm <- lm( gsi ~ TempF + PhotoF + TempF:PhotoF, data=gsi)
cat("\n\n Analysis of variance -- this is correct because design is balanced \n")
anova(result.lm)
##***part100e;
sink()

##***part100diagnosticb;
# Check the residuals etc
resid.plot <- sf.autoplot.lm(result.lm)
resid.plot
##***part100diagnostice;

ggsave(plot=resid.plot, file='gsi-R-100-diagnostic.png', height=4, width=6, units="in", dpi=300)


# LSmeans after a lm() fit
sink('gsi-R-100LSM-Temp.txt', split=TRUE)
##***part100LSM-Tempb;
result.Temp.lsmo <- lsmeans::lsmeans(result.lm, ~TempF, 
                              adjust='tukey')
cat("\n\n Estimated marginal means for Temperature levels \n\n")
summary(result.Temp.lsmo, infer=TRUE, adjust='tukey')
##***part100LSM-Tempe;
sink()

sink('gsi-R-100LSM-Photo.txt', split=TRUE)
##***part100LSM-Photob;
result.Photo.lsmo <- lsmeans:: lsmeans(result.lm, ~PhotoF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo period levels \n\n")
summary(result.Photo.lsmo, infer=TRUE, adjust='tukey')
##***part100LSM-Photoe;
sink()

sink('gsi-R-100LSM-int.txt', split=TRUE)
##***part100LSM-intb;
result.PhotoTemp.lsmo <- lsmeans:: lsmeans(result.lm, ~PhotoF:TempF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo period x Temp combinations \n\n")
summary(result.PhotoTemp.lsmo, infer=TRUE, adjust='tukey')
##***part100LSM-inte;
sink()


# Do the multiple comparisons
# In this case, each factor only has two levels so not very interesting and
# is equivalent to ordinary t-tests. You also need to be careful
# of comparing main effects because of the large interaction
sink('gsi-R-100multcomp-Temp.txt', split=TRUE)
##***part100multcomp-Tempb;
cat("\n\n Multiple comparison for Temp effect\n")
result.Temp.cld <- cld(result.Temp.lsmo)
result.Temp.cld
Temp.cld.plot <- sf.cld.plot.bar(result.Temp.cld, "TempF")+
   ggtitle("Comparing mean GSI over Temp levels")+
   xlab("Temperature\nNumbers indicated CLD display")+ylab("Mean GSI")
Temp.cld.plot

# Estimate the pairwise differences
pairs(result.Temp.lsmo)
confint(pairs(result.Temp.lsmo))
##***part100multcomp-Tempe;
sink()

ggsave(plot=Temp.cld.plot, file='gsi-R-100multcomp-Temp.png', height=4, width=6, units="in", dpi=300)


# Ditto for the Photo period effect
sink('gsi-R-100multcomp-Photo.txt', split=TRUE)

cat("\n\n Multiple comparison for Photo period effect\n")
result.Photo.cld <- cld(result.Photo.lsmo)
result.Photo.cld
Photo.cld.plot <- sf.cld.plot.bar(result.Photo.cld, "PhotoF")+
   ggtitle("Comparing mean GSI over Photo Period levels")+
   xlab("Photo Period\nNumbers indicated CLD display")+ylab("Mean GSI")
Photo.cld.plot

# Estimate the pairwise differences
pairs(result.Photo.lsmo)
confint(pairs(result.Photo.lsmo))
##***part100multcomp-Photoe;
sink()

ggsave(plot=Photo.cld.plot, file='gsi-R-100multcomp-Photo.png', height=4, width=6, units="in", dpi=300)



# And for all combinations of PhotoPeriod and Temperature
sink('gsi-R-100multcomp-int.txt', split=TRUE)
##***part100multcomp-intb;
cat("\n\n Multiple comparison for Temp and Photo period effect\n")
result.PhotoTemp.cld <- cld(result.PhotoTemp.lsmo)
result.PhotoTemp.cld$trtF <- interaction(result.PhotoTemp.cld$PhotoF,
                                         result.PhotoTemp.cld$TempF)
result.PhotoTemp.cld
PhotoTemp.cld.plot <- sf.cld.plot.bar(result.PhotoTemp.cld, "trtF")+
   ggtitle("Comparing mean GSI over Photo Period x Temperature levels")+
   xlab("Photo Period x Temperature \nNumbers indicated CLD display")+ylab("Mean GSI")
PhotoTemp.cld.plot

# Estimate the pairwise differences
pairs(result.PhotoTemp.lsmo)
confint(pairs(result.PhotoTemp.lsmo))
##***part100multcomp-inte;
sink()

ggsave(plot=PhotoTemp.cld.plot, file='gsi-R-100multcomp-int.png', height=4, width=6, units="in", dpi=300)



# Because the interaction is large, we may wish to do slices where we look a one factor at each
# level of the other factor. THis is again easily done using the lsmeans package

sink('gsi-R-100multcomp-TempByPhoto.txt', split=TRUE)
##***part100multcomp-TempByPhotob;
cat("\n\n Multiple comparison for Temp effect sliced by Photo Period\n")
result.TempByPhoto.lsmo <- lsmeans:: lsmeans(result.lm, ~TempF | PhotoF, adjust='tukey')
cat("\n\n Estimated marginal means for Temperature levels sliced by Photo Period \n\n")
summary(result.TempByPhoto.lsmo, infer=TRUE, adjust='tukey')

result.TempByPhoto.cld <- cld(result.TempByPhoto.lsmo)
result.TempByPhoto.cld

# Estimate the pairwise differences
pairs(result.TempByPhoto.lsmo)
confint(pairs(result.TempByPhoto.lsmo))
##***part100multcomp-TempByPhotoe;
sink()

# Or the reverse slice
sink('gsi-R-100multcomp-PhotoByTemp.txt', split=TRUE)
##***part100multcomp-PhotoByTempb;
cat("\n\n Multiple comparison for Photo Period effect sliced by Temperature \n")
result.PhotoByTemp.lsmo <- lsmeans:: lsmeans(result.lm, ~PhotoF | TempF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo Period levels sliced by Temperatuer \n\n")
summary(result.PhotoByTemp.lsmo, infer=TRUE, adjust='tukey')

result.PhotoByTemp.cld <- cld(result.PhotoByTemp.lsmo)
result.PhotoByTemp.cld

# Estimate the pairwise differences
pairs(result.PhotoByTemp.lsmo)
confint(pairs(result.PhotoByTemp.lsmo))
##***part100multcomp-PhotoByTempe;
sink()





#*********************************************************************************
# ******************* REMOVE SOME OBSERVATIONS to make the design unbalanced ****
#*********************************************************************************


cat("\n\n ************* UNBALANCED DATA ******************\n\n")
cat(" Remove some of the data points \n")
gsi[gsi$Temp== '27C' & gsi$Photo=='09h' & gsi$gsi < 1,"gsi"] <- NA
gsi[gsi$Temp== '16C' & gsi$Photo=='14h' & gsi$gsi > 1.3,"gsi"] <- NA
gsi
gsi <- gsi[! is.na(gsi$gsi),]  # remove all of the missing values from the analysis


# Get some simple summary statistics
sink('gsi-R-520.txt', split=TRUE)
##***part520b;
report <- ddply(gsi, c("PhotoF","TempF"),sf.simple.summary, variable="gsi", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part520e;
sink()


# Fit the linear model, get the anova table, and the usual stuff
# CAUTION!!! Because the design is unbalance, the default model
# fit by aov gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html
#
# You can also use the Anova() function from the car package but also need to set the contrast matrix.

cat("The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
result.lm <- lm( gsi ~ TempF + PhotoF + TempF:PhotoF, data=gsi)
cat("\n\n Analysis of variance -- this is NOT CORRECT because design is unbalanced \n")
anova(result.lm)

cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat(  "\nwithin the lm() model!")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")

result.lm2 <- lm( gsi ~ TempF + PhotoF + TempF:PhotoF, data=gsi,
                  contrasts=list(TempF='contr.sum', PhotoF='contr.sum'))
                  #contrasts=c(unordered="contr.sum", ordered="contr.poly"))
Anova(result.lm2,type=3)




##***part600diagnosticb;
# Check the residuals etc
resid.plot <- sf.autoplot.lm(result.lm2)
resid.plot
##***part600diagnostice;

ggsave(plot=resid.plot, file='gsi-R-600-diagnostic.png', height=4, width=6, units="in", dpi=300)


# LSmeans after a lm() fit
# These are still ok even if the design is unbalanced and the incremental SS are found
sink('gsi-R-600LSM-Temp.txt', split=TRUE)
##***part600LSM-Tempb;
result.Temp.lsmo <- lsmeans:: lsmeans(result.lm2, ~TempF, adjust='tukey')
cat("\n\n Estimated marginal means for Temperature levels \n\n")
summary(result.Temp.lsmo, infer=TRUE, adjust='tukey')
##***part600LSM-Tempe;
sink()

sink('gsi-R-600LSM-Photo.txt', split=TRUE)
##***part600LSM-Photob;
result.Photo.lsmo <- lsmeans:: lsmeans(result.lm2, ~PhotoF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo period levels \n\n")
summary(result.Photo.lsmo, infer=TRUE, adjust='tukey')
##***part600LSM-Photoe;
sink()

sink('gsi-R-600LSM-int.txt', split=TRUE)
##***part600LSM-intb;
result.PhotoTemp.lsmo <- lsmeans:: lsmeans(result.lm2, ~PhotoF:TempF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo period x Temp combinations \n\n")
summary(result.PhotoTemp.lsmo, infer=TRUE, adjust='tukey')
##***part600LSM-inte;
sink()


# Do the multiple comparisons
# In this case, each factor only has two levels so not very interesting and
# is equivalent to ordinary t-tests. You also need to be careful
# of comparing main effects because of the large interaction
sink('gsi-R-600multcomp-Temp.txt', split=TRUE)
##***part600multcomp-Tempb;
cat("\n\n Multiple comparison for Temp effect\n")
result.Temp.cld <- cld(result.Temp.lsmo)
result.Temp.cld
Temp.cld.plot <- sf.cld.plot.bar(result.Temp.cld, "TempF")+
   ggtitle("Comparing mean GSI over Temp levels")+
   xlab("Temperature\nNumbers indicated CLD display")+ylab("Mean GSI")
Temp.cld.plot

# Estimate the pairwise differences
pairs(result.Temp.lsmo)
confint(pairs(result.Temp.lsmo))
##***part600multcomp-Tempe;
sink()

ggsave(plot=Temp.cld.plot, file='gsi-R-600multcomp-Temp.png', height=4, width=6, units="in", dpi=300)


# Ditto for the Photo period effect
sink('gsi-R-600multcomp-Photo.txt', split=TRUE)

cat("\n\n Multiple comparison for Photo period effect\n")
result.Photo.cld <- cld(result.Photo.lsmo)
result.Photo.cld
Photo.cld.plot <- sf.cld.plot.bar(result.Photo.cld, "PhotoF")+
   ggtitle("Comparing mean GSI over Photo Period levels")+
   xlab("Photo Period\nNumbers indicated CLD display")+ylab("Mean GSI")
Photo.cld.plot

# Estimate the pairwise differences
pairs(result.Photo.lsmo)
confint(pairs(result.Photo.lsmo))
##***part600multcomp-Photoe;
sink()

ggsave(plot=Photo.cld.plot, file='gsi-R-600multcomp-Photo.png', height=4, width=6, units="in", dpi=300)



# And for all combinations of PhotoPeriod and Temperature
sink('gsi-R-600multcomp-int.txt', split=TRUE)
##***part600multcomp-intb;
cat("\n\n Multiple comparison for Temp and Photo period effect\n")
result.PhotoTemp.cld <- cld(result.PhotoTemp.lsmo)
result.PhotoTemp.cld$trtF <- interaction(result.PhotoTemp.cld$PhotoF,
                                         result.PhotoTemp.cld$TempF)
result.PhotoTemp.cld
PhotoTemp.cld.plot <- sf.cld.plot.bar(result.PhotoTemp.cld, "trtF")+
   ggtitle("Comparing mean GSI over Photo Period x Temperature levels")+
   xlab("Photo Period x Temperature \nNumbers indicated CLD display")+ylab("Mean GSI")
PhotoTemp.cld.plot

# Estimate the pairwise differences
pairs(result.PhotoTemp.lsmo)
confint(pairs(result.PhotoTemp.lsmo))
##***part600multcomp-inte;
sink()

ggsave(plot=PhotoTemp.cld.plot, file='gsi-R-600multcomp-int.png', height=4, width=6, units="in", dpi=300)


# Because the interaction is large, we may wish to do slices where we look a one factor at each
# level of the other factor. THis is again easily done using the lsmeans package

sink('gsi-R-600multcomp-TempByPhoto.txt', split=TRUE)
##***part600multcomp-TempByPhotob;
cat("\n\n Multiple comparison for Temp effect sliced by Photo Period\n")
result.TempByPhoto.lsmo <- lsmeans:: lsmeans(result.lm2, ~TempF | PhotoF, adjust='tukey')
cat("\n\n Estimated marginal means for Temperature levels sliced by Photo Period \n\n")
summary(result.TempByPhoto.lsmo, infer=TRUE, adjust='tukey')

result.TempByPhoto.cld <- cld(result.TempByPhoto.lsmo)
result.TempByPhoto.cld

# Estimate the pairwise differences
pairs(result.TempByPhoto.lsmo)
confint(pairs(result.TempByPhoto.lsmo))
##***part600multcomp-TempByPhotoe;
sink()

# Or the reverse slice
sink('gsi-R-600multcomp-PhotoByTemp.txt', split=TRUE)
##***part600multcomp-PhotoByTempb;
cat("\n\n Multiple comparison for Photo Period effect sliced by Temperature \n")
result.PhotoByTemp.lsmo <- lsmeans:: lsmeans(result.lm2, ~PhotoF | TempF, adjust='tukey')
cat("\n\n Estimated marginal means for Photo Period levels sliced by Temperatuer \n\n")
summary(result.PhotoByTemp.lsmo, infer=TRUE, adjust='tukey')

result.PhotoByTemp.cld <- cld(result.PhotoByTemp.lsmo)
result.PhotoByTemp.cld

# Estimate the pairwise differences
pairs(result.PhotoByTemp.lsmo)
confint(pairs(result.PhotoByTemp.lsmo))
##***part600multcomp-PhotoByTempe;
sink()


# The usual diagnostics
sf.autoplot.lm(result.lm2)




