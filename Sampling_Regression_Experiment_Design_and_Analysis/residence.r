						
# Stream residence time of salmon
# 2015-07-10 CJS Update with split; ggplot2; lsmemans; contrasts etc

# The stream residence of time was measured for individually tagged fish in 
# a number of YearFs.

# Example of a two factor CRD analysis of variance with unbalanced data

options(useFancyQuotes=FALSE) # renders summary output corrects
library(car)
library(ggplot2)
library(lsmeans)
library(plyr)

source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")


# Read in the actual data
sink("residence-R-001.txt", split=TRUE)
##***part001b;
cat(" Effect of Sex and YearF on residence time levels in Fish \n\n")

restime <- read.csv("residence.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
restime$SexF  <- factor(restime$Sex)
restime$YearF <- as.factor(restime$Year)
restime$trtF  <- interaction(restime$Sex, restime$Year)

cat("Listing of part of the raw data \n")
head(restime)
##***part001e;
sink()


str(restime)


# Preliminary plot

# Get side-by-side dot plots
# Get side-by-side dot plots
##***part010b;
plot1 <- ggplot(data=restime, aes(x=trtF, y=ResidenceTime))+
  ggtitle("Residence Time levels in different sex/year combinations")+
  ylab("Residence Time")+xlab("Sex and year")+
  geom_point( position=position_jitter(width=0.2), size=4)+
  geom_boxplot(alpha=0.2, width=0.5, notch=TRUE)
plot1
##***part010e;
ggsave(plot=plot1, file="residence-R-010.png", height=4, width=6, units="in", dpi=300)



# Get some simple summary statistics
sink('residence-R-020.txt', split=TRUE)
##***part020b;
report <- ddply(restime, c("SexF","YearF"),sf.simple.summary, variable="ResidenceTime", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part020e;
sink()

# Draw a profile plot
##***part030b;
profile.plot <-  ggplot(data=report, aes(x=SexF, y=mean, group=YearF, shape=YearF))+
  ggtitle("Profile plot of mean ResidenceTime scores")+
  ylab("Mean ResidenceTime with 95% ci")+xlab('year')+
  geom_point(size=4, position=position_dodge(.1))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.1, position=position_dodge(.1))+
  geom_line(position=position_dodge(.1), aes(linetype=YearF))
profile.plot
##***part030e;

ggsave(plot=profile.plot, file="residence-R-030.png", height=4, width=6, unit="in", dpi=300)  # send the plot to a png file




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
sink('residence-R-100.txt', split=TRUE)
##***part100b;
cat("The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
result.lm <- lm( ResidenceTime ~ SexF + YearF + SexF*YearF, data=restime)
cat("\n\n Analysis of variance -- this is NOT CORRECT because design is unbalanced \n")
anova(result.lm)

cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")

result.lm2 <- lm( ResidenceTime ~ SexF + YearF + SexF*YearF, data=restime,
                  contrasts=list(SexF="contr.sum", YearF="contr.sum"))
Anova(result.lm2,type=3)
##***part100e;
sink()



##***part100diagnosticb;
diagplot <- sf.autoplot.lm(result.lm2)
diagplot
diagplot
##***part100diagnostice;
ggsave(plot=diagplot, file='residence-R-100-diagnostic.png', h=6, w=6, units='in', dpi=300)





# LSmeans for sex after a lm() fit
sink('residence-R-100LSM-sex.txt', split=-TRUE)
##***part100LSM-sexb;
result.sex.lsmo <- lsmeans::lsmeans(result.lm2, ~SexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.sex.lsmo, infer=TRUE)
##***part100LSM-sexe;
sink()

sink('residence-R-100LSM-year.txt', split=TRUE)
##***part100LSM-yearb;
cat("\n\n Estimated marginal means \n\n")
result.year.lsmo <- lsmeans::lsmeans(result.lm, ~YearF)
cat("\n\n Estimated marginal means \n\n")
summary(result.year.lsmo, infer=TRUE)
##***part100LSM-yeare;
sink()

sink('residence-R-100LSM-int.txt', split=TRUE)
##***part100LSM-intb;
result.year.sex.lsmo <- lsmeans::lsmeans(result.lm, ~YearF:SexF)
cat("\n\n Estimated marginal means \n\n")
summary(result.year.sex.lsmo, infer=TRUE)
##***part100LSM-inte;
sink()







# Do the multiple comparisons using the compact letter display from the lsmeans package
sink('residence-R-100multcomp-sex.txt', split=TRUE)
##***part100multcomp-sexb;
cat("\n\n Multiple comparison for Sex effect \n")
summary(pairs(result.sex.lsmo), infer=TRUE)
result.sex.cld <- cld(result.sex.lsmo)
result.sex.cld
sink()

sex.cld.plot <- sf.cld.plot.bar(result.sex.cld, "SexF")+
  ggtitle("Comparing mean ResidenceTime over sex levels")+
  xlab("Sex\nNumbers indicated CLD display")+ylab("Mean ResidenceTime")
sex.cld.plot
##***part100multcomp-sexe;

ggsave(plot=sex.cld.plot, file='residence-R-100multcomp-sex.png', h=4, w=6, units="in", dpi=300)



sink('residence-R-100multcomp-year.txt', split=TRUE)
##***part100multcomp-yearb;
cat("\n\n Multiple comparison for year effect \n")
summary(pairs(result.year.lsmo), infer=TRUE)
result.year.cld <- cld(result.year.lsmo)
result.year.cld
sink()

year.cld.plot <- sf.cld.plot.bar(result.year.cld, "YearF")+
  ggtitle("Comparing mean ResidenceTime over year levels")+
  xlab("year\nNumbers indicated CLD display")+ylab("Mean ResidenceTime")
year.cld.plot
##***part100multcomp-yeare;

ggsave(plot=year.cld.plot, file='residence-R-100multcomp-year.png', h=4, w=6, units="in", dpi=300)


sink('residence-R-100multcomp-year-sex.txt', split=TRUE)
##***part100multcomp-sexyearb;
# Sex x year terms.
cat("\n\n Multiple comparison for year-Sex combinations \n")
summary(pairs(result.year.sex.lsmo), infer=TRUE)
result.year.sex.cld <- cld(result.year.sex.lsmo)
result.year.sex.cld
sink()

result.year.sex.cld$trt <- interaction(result.year.sex.cld$YearF, result.year.sex.cld$SexF)
year.sex.cld.plot <- sf.cld.plot.bar(result.year.sex.cld, "trt")+
  ggtitle("Comparing mean ResidenceTime over year/Sex levels")+
  xlab("year-Sex\nNumbers indicated CLD display")+ylab("Mean ResidenceTime")
year.sex.cld.plot
##***part100multcomp-sexyeare;
ggsave(plot=year.cld.plot, file='residence-R-100multcomp-year-sex.png', h=4, w=6, units="in", dpi=300)

