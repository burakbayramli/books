# Different PCB concentrations by sex and species of fish
# 2015-07-10 CJS  sink, ggplot, type III etc

# Example of a two factor CRD analysis of variance
# using both balanced and unbalanced data

#  
# Effect of sex and species upon chemical uptake}
#
# Several persistent chemicals accumulate up the food chain. 
# Different species may vary in the amount 
# of chemicals accumulated because of different prey availability
# or other factors.  Because of different behavior, 
# the amount may also vary by sex.

# A survey was conducted to investigate how the amount of PCBs varied
# among three different species of fish in Nunavut (the new Canadian 
# territory just to east of the Restofit and just north of Ulofit).
# Samples were taken from four fish of each sex and species and the PCB
# levels (PPM) measured in the livers. 

options(useFancyQuotes=FALSE) # renders summary output corrects
library(car)
library(ggplot2)
library(lsmeans)
library(plyr)

source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the actual data
sink("pcb-R-001.txt", split=TRUE)
##***part001b;
cat(" Effect of Sex and Species on PCB levels in Fish \n\n")

pcb <- read.csv("pcb.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
# Create factors
pcb$sexF     <- factor(pcb$sex)
pcb$speciesF <- factor(pcb$species)
pcb$trtF     <- interaction(pcb$sex, pcb$species)

cat("Listing of part of the raw data \n")
head(pcb)
##***part001e;
sink()

str(pcb)
# Preliminary plot

# Get side-by-side dot plots
##***part010b;
plot1 <- ggplot(data=pcb, aes(x=trtF, y=pcb))+
  ggtitle("PCB levels in different sex/species combinations")+
  ylab("PCB")+xlab("Sex and Species")+
  geom_point( position=position_jitter(width=0.2), size=4)+
  geom_boxplot(alpha=0.2, width=0.5, notch=TRUE)
plot1
##***part010e;
ggsave(plot=plot1, file="pcb-R-010.png", height=4, width=6, units="in", dpi=300)



# Get some simple summary statistics
sink('pcb-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(pcb, c("sexF","speciesF"),sf.simple.summary, variable="pcb", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part020e;
sink()

# Draw a profile plot
##***part030b;
profile.plot <-  ggplot(data=report, aes(x=sexF, y=mean, group=speciesF, shape=speciesF))+
  ggtitle("Profile plot of mean PCB scores")+
  ylab("Mean PCB with 95% ci")+xlab('Species')+
  geom_point(size=4, position=position_dodge(.1))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.1, position=position_dodge(.1))+
  geom_line(position=position_dodge(.1), aes(linetype=speciesF))
profile.plot
##***part030e;

ggsave(plot=profile.plot, file="pcb-R-030.png", height=4, width=6, unit="in", dpi=300)  # send the plot to a png file



# See if a transformation is needed
# Plot the log(std dev) vs. the log(mean) to see if transformation needed
#   under Taylor's Power Law 
sink('pcb-R-040.txt', split=TRUE)
taylor.plot <- ggplot(data=report, aes(x=log(mean), y=log(sd)))+
  ggtitle("Log(sd) vs log(mean) to see if transformation needed")+
  xlab("log(mean) of pcb")+ylab("log(sd) of pcb")+
  geom_point(size=4)+
  stat_smooth(method="lm", se=FALSE)
taylor.plot 
cat("\n\n Fitted line to log(sd) vs log(mean) \n")
summary(lm( log(sd) ~ log(mean), data=report))
sink()
ggsave(plot=taylor.plot, file="pcb-R-040.png", height=4, width=6, units="in", dpi=300)  # send the plot to a png file




# Fit the linear model, get the anova table, and the usual stuff
# Because the design is BALANCED, R produces the correct sums of squares
# See the entry further down in this program
# about cautions with unbalanced data
sink('pcb-R-100.txt', split=TRUE)
##***part100b;
result.lm <- lm( pcb ~ sexF + speciesF + sexF:speciesF, data=pcb)
cat("\n\n Analysis of variance -- this is correct because design is balanced \n")
anova(result.lm)
##***part100e;
sink()

##***part100diagnosticb;
diagplot <- sf.autoplot.lm(result.lm)
##***part100diagnostice;
ggsave(plot=diagplot, file='pcb-R-100-diagnostic.png', h=6, w=6, units='in', dpi=300)



# LSmeans for sex after a lm() fit
sink('pcb-R-100LSM-sex.txt', split=-TRUE)
##***part100LSM-sexb;
result.sex.lsmo <- lsmeans::lsmeans(result.lm, ~sexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.sex.lsmo, infer=TRUE)
##***part100LSM-sexe;
sink()

sink('pcb-R-100LSM-species.txt', split=TRUE)
##***part100LSM-speciesb;
cat("\n\n Estimated marginal means \n\n")
result.species.lsmo <- lsmeans::lsmeans(result.lm, ~speciesF)
cat("\n\n Estimated marginal means \n\n")
summary(result.species.lsmo, infer=TRUE)
##***part100LSM-speciese;
sink()

sink('pcb-R-100LSM-int.txt', split=TRUE)
##***part100LSM-intb;
result.species.sex.lsmo <- lsmeans::lsmeans(result.lm, ~speciesF:sexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.species.sex.lsmo, infer=TRUE)
##***part100LSM-inte;
sink()


# Do the multiple comparisons using the compact letter display from the lsmeans package
sink('pcb-R-100multcomp-sex.txt', split=TRUE)
##***part100multcomp-sexb;
cat("\n\n Multiple comparison for Sex effect \n")
summary(pairs(result.sex.lsmo), infer=TRUE)
result.sex.cld <- cld(result.sex.lsmo)
result.sex.cld
sink()

sex.cld.plot <- sf.cld.plot.bar(result.sex.cld, "sexF")+
  ggtitle("Comparing mean pcb over sex levels")+
  xlab("Sex\nNumbers indicated CLD display")+ylab("Mean pcb")
sex.cld.plot
##***part100multcomp-sexe;

ggsave(plot=sex.cld.plot, file='pcb-R-100multcomp-sex.png', h=4, w=6, units="in", dpi=300)



sink('pcb-R-100multcomp-species.txt', split=TRUE)
##***part100multcomp-speciesb;
cat("\n\n Multiple comparison for Species effect \n")
summary(pairs(result.species.lsmo), infer=TRUE)
result.species.cld <- cld(result.species.lsmo)
result.species.cld
sink()

species.cld.plot <- sf.cld.plot.bar(result.species.cld, "speciesF")+
  ggtitle("Comparing mean pcb over Species levels")+
  xlab("Species\nNumbers indicated CLD display")+ylab("Mean pcb")
species.cld.plot
##***part100multcomp-speciese;

ggsave(plot=species.cld.plot, file='pcb-R-100multcomp-species.png', h=4, w=6, units="in", dpi=300)

sink('pcb-R-100multcomp-species-sex.txt', split=TRUE)
##***part100multcomp-sexspeciesb;
# Sex x Species terms.
cat("\n\n Multiple comparison for Species-Sex combinations \n")
summary(pairs(result.species.sex.lsmo), infer=TRUE)
result.species.sex.cld <- cld(result.species.sex.lsmo)
result.species.sex.cld
sink()

result.species.sex.cld$trt <- interaction(result.species.sex.cld$speciesF, result.species.sex.cld$sexF)
species.sex.cld.plot <- sf.cld.plot.bar(result.species.sex.cld, "trt")+
  ggtitle("Comparing mean pcb over Species/Sex levels")+
  xlab("Species-Sex\nNumbers indicated CLD display")+ylab("Mean pcb")
species.sex.cld.plot
##***part100multcomp-sexspeciese;
ggsave(plot=species.cld.plot, file='pcb-R-100multcomp-species-sex.png', h=4, w=6, units="in", dpi=300)



#*********************************************************************************
# ******************* REMOVE SOME OBSERVATIONS to make the design unbalanced ****
#*********************************************************************************


cat("\n\n ************* UNBALANCED DATA ******************\n\n")
cat(" Remove some of the data points \n")
pcb[pcb$sex== 'm' & pcb$species=='sp1' & pcb$pcb < 21,"pcb"] <- NA
pcb[pcb$sex== 'f' & pcb$species=='sp3' & pcb$pcb > 14,"pcb"] <- NA
pcb
pcb <- pcb[! is.na(pcb$pcb),]  # remove all of the missing values from the analysis



# Get some simple summary statistics
report <- ddply(pcb, c("sexF","speciesF"),sf.simple.summary, variable="pcb", crd=TRUE)
cat("\n\n Summary report \n")
report


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
# You can also use the Anova() function from the car package.

cat("The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
result.lm <- lm( pcb ~ sexF + speciesF + sexF:speciesF, data=pcb)
cat("\n\n Analysis of variance -- this is NOT CORRECT because design is unbalanced \n")
anova(result.lm)

cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat(  "\nBEFORE fitting the lm() model!")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")
library(car)
result.lm2 <- lm( pcb ~ sexF + speciesF + sexF:speciesF, data=pcb,
                  contrasts=list(sexF='contr.sum', speciesF='contr.sum'))
Anova(result.lm2,type=3)

##***part600diagnosticb;
diagplot <- sf.autoplot.lm(result.lm2)
##***part600diagnostice;
ggsave(plot=diagplot, file='pcb-R-600-diagnostic.png', h=6, w=6, units='in', dpi=300)



# LSmeans for sex after a lm() fit
sink('pcb-R-600LSM-sex.txt', split=-TRUE)
##***part600LSM-sexb;
result.sex.lsmo <- lsmeans::lsmeans(result.lm2, ~sexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.sex.lsmo, infer=TRUE)
##***part600LSM-sexe;
sink()

sink('pcb-R-600LSM-species.txt', split=TRUE)
##***part600LSM-speciesb;
cat("\n\n Estimated marginal means \n\n")
result.species.lsmo <- lsmeans::lsmeans(result.lm2, ~speciesF)
cat("\n\n Estimated marginal means \n\n")
summary(result.species.lsmo, infer=TRUE)
##***part600LSM-speciese;
sink()

sink('pcb-R-600LSM-int.txt', split=TRUE)
##***part600LSM-intb;
result.species.sex.lsmo <- lsmeans::lsmeans(result.lm2, ~speciesF:sexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.species.sex.lsmo, infer=TRUE)
##***part600LSM-inte;
sink()


# Do the multiple comparisons using the compact letter display from the lsmeans package
sink('pcb-R-600multcomp-sex.txt', split=TRUE)
##***part600multcomp-sexb;
cat("\n\n Multiple comparison for Sex effect \n")
summary(pairs(result.sex.lsmo), infer=TRUE)
result.sex.cld <- cld(result.sex.lsmo)
result.sex.cld
sink()

sex.cld.plot <- sf.cld.plot.bar(result.sex.cld, "sexF")+
  ggtitle("Comparing mean pcb over sex levels")+
  xlab("Sex\nNumbers indicated CLD display")+ylab("Mean pcb")
sex.cld.plot
##***part600multcomp-sexe;

ggsave(plot=sex.cld.plot, file='pcb-R-600multcomp-sex.png', h=4, w=6, units="in", dpi=300)



sink('pcb-R-600multcomp-species.txt', split=TRUE)
##***part600multcomp-speciesb;
cat("\n\n Multiple comparison for Species effect \n")
summary(pairs(result.species.lsmo), infer=TRUE)
result.species.cld <- cld(result.species.lsmo)
result.species.cld
sink()

species.cld.plot <- sf.cld.plot.bar(result.species.cld, "speciesF")+
  ggtitle("Comparing mean pcb over Species levels")+
  xlab("Species\nNumbers indicated CLD display")+ylab("Mean pcb")
species.cld.plot
##***part600multcomp-speciese;

ggsave(plot=species.cld.plot, file='pcb-R-600multcomp-species.png', h=4, w=6, units="in", dpi=300)

sink('pcb-R-600multcomp-species-sex.txt', split=TRUE)
##***part600multcomp-sexspeciesb;
# Sex x Species terms.
cat("\n\n Multiple comparison for Species-Sex combinations \n")
summary(pairs(result.species.sex.lsmo), infer=TRUE)
result.species.sex.cld <- cld(result.species.sex.lsmo)
result.species.sex.cld
sink()

result.species.sex.cld$trt <- interaction(result.species.sex.cld$speciesF, result.species.sex.cld$sexF)
species.sex.cld.plot <- sf.cld.plot.bar(result.species.sex.cld, "trt")+
  ggtitle("Comparing mean pcb over Species/Sex levels")+
  xlab("Species-Sex\nNumbers indicated CLD display")+ylab("Mean pcb")
species.sex.cld.plot
##***part600multcomp-sexspeciese;
ggsave(plot=species.cld.plot, file='pcb-R-600multcomp-species-sex.png', h=4, w=6, units="in", dpi=300)



#



