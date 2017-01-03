# Use-Dependent Inactivation in Sodium Channel Beta Subunit Mutation

# This dataset was provided by Csilla Egri as part of a 2011 M.Sc.
# Thesis C121W: A thermosensitive sodium channel mutation''. 
#  Additional # details are available at
#  http://dx.doi.org/10.1016/j.bpj.2010.12.2506.

#
# Voltage gated sodium (NaV) channels are macromolecular complexes 
# which pass sodium specific inward current and are the main determinants 
# of action potential in initiation and propagation. 
# NaV normally associate with one or more auxiliary beta subunits 
# which modify voltage dependent properties. 
# Mutations to these proteins can cause epilepsy, 
# cardiac arrhythmias, and skeletal muscle disorders. 

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(car)
library(ggplot2)
library(lsmeans)
library(plyr)

source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r') # load the helper functions



# Read in the data
sink('UDI-R-001.txt', split=TRUE)
##***part001b;
UDI <- read.csv('UDI.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
UDI$SubunitF  <- factor(UDI$Subunit)
UDI$TempF     <- factor(UDI$Temp)
UDI$trtF      <- interaction(UDI$Subunit, UDI$Temp)
head(UDI)
##***part001e;
sink()

# Get side-by-side dot plots. 
##***part004b;
plot010 <- ggplot(data=UDI, aes(x=trtF, y=UDI_Yo))+
  geom_jitter(size=3, position=position_jitter(width=0.2, height=0.1))+
  geom_boxplot(alpha=0.2, notch=TRUE)+
  xlab("Treatment\n Point jittered to prevent overplotting")+
  ylab("UDI_Yo")+
  ggtitle("UDI with overlaid boxplots")
plot010
##***part004e;
ggsave(plot010, file="UDI-R-004.png", height=4, width=6, units="in", dpi=300) 



# Compute the means and standard deviations and sample sizes for
# each treatment group

sink('UDI-R-002.txt', split=TRUE)
##***part002b;
##***part020b;
report <- ddply(UDI, c("SubunitF","TempF"), sf.simple.summary, variable="UDI_Yo", crd=TRUE)
cat("\n\n Summary report \n")
report
##***part002e;
sink()


# Draw a profile plot based on the summary data generated
##***part030b;
plot030 <- ggplot(report, aes(x=TempF, y=mean, group=SubunitF, color=SubunitF, linetype=SubunitF))+
  ggtitle("Interaction plot based on raw data")+
  xlab("Temperaturet \n Jittering applied to prevent overplotting")+ylab("UDI with 95% ci")+
  geom_pointrange(aes(ymin=lcl, ymax=ucl),position = position_dodge(0.05),size=1)+
  geom_line(size=2)
plot030
##***part030e;

ggsave(plot030, file="UDI-R-021.png", height=4, width=6, units="in")  # send the plot to a png file




# Plot the standard deviation vs the mean to see if there
# is a relationship
##***part003b;
plot011 <- ggplot(report, aes(x=log(mean), y=log(sd)))+
  ggtitle("See if the SD increases with the mean")+
  xlab("log(Mean UDI)")+
  ylab("log(SD UDI)")+
  geom_point(size=3)
plot011
##***part003e;
ggsave(plot=plot011, file='UDI-R-003.png', h=4, w=6, units="in", dpi=300)




# Fit the linear model and get the ANOVA table and test for effects
# Be sure that Temperature is treated as a factor
#
# CAUTION!!! Because the design is unbalance, the default model
# fit by aov gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html

sink('UDI-R-014.txt', split=TRUE)
##***part014b;
cat("The sum of squares and F-tests below are INCORRECT\n")
cat("because they are sequential and only adjust for effect\n")
cat("that enter the model prior to the term in question.")
wrong.result <- lm(UDI_Yo ~ TempF+SubunitF+  TempF*Subunit, data=UDI)
anova(wrong.result)
##***part014e;
sink()

sink('UDI-R-015.txt', split=TRUE)
##***part015b;
cat('We need to adjust the internal contrast matrix\n')
cat('to get the marginal sums of squares\n')
cat("")
cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat(  "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat("  \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html")
cat("")
result.lm2 <- lm( UDI_Yo ~ TempF + SubunitF + TempF:SubunitF, data=UDI,
                  contrasts=list(TempF="contr.sum", SubunitF="contr.sum"))
Anova(result.lm2,type=3)
##***part015e;
sink()


# Check the residuals etc using the schwarz autoplot functions
##***part100diagnosticb;
plotdiag <-sf.autoplot.lm(result.lm2, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***part100diagnostice;
ggsave(plot=plotdiag, file='UDI-R-020.png', height=6, width=6, units="in")




# Estimate the marginal means using the doBy package
# You must refit the model using the lm() function.
# We also force Temperature (which is numeric) to be a factor.

sink("UDI-R-025.txt")
##***part025b;
temp.lsmo <- lsmeans::lsmeans(result.lm2, ~TempF)
summary(temp.lsmo, infer=TRUE)
subunit.lsmo <- lsmeans::lsmeans(result.lm2, ~SubunitF)
summary(subunit.lsmo, infer=TRUE)
temp.subunit.lsmo <- lsmeans::lsmeans(result.lm2, ~TempF:SubunitF)
summary(temp.subunit.lsmo, infer=TRUE)
##***part025e;
sink()


# Now for a multiple comparison procedure to estimate the differences between the lsmeans


sink('UDI-R-030.txt', split=TRUE)
##***part030b;
temp.cld <- cld(temp.lsmo)
temp.cld
summary(pairs(temp.lsmo), infer=TRUE)


temp.cld.plot <- sf.cld.plot.bar(temp.cld, variable="TempF")
temp.cld.plot
##***part030e;
sink()
ggsave(plot=temp.cld.plot, file='UDI-R-030.png', h=4, w=6, units="in", dpi=300)




# Similar analysis for the subunits
subunit.cld <- cld(subunit.lsmo)
subunit.cld
summary(pairs(subunit.lsmo), infer=TRUE)


subunit.cld.plot <- sf.cld.plot.bar(subunit.cld, variable="SubunitF")
subunit.cld.plot


# A specialized contrast comparing  NaV1.2+B(CW) and NAV1.2 sub-unit # types at the elevated temperature
# can be formed using the 

sink('UDI-R-040.txt', split=TRUE)
##***part040b;
summary(contrast(temp.subunit.lsmo,
         list(c1=c(0,0,0,1,0,-1))), infer=TRUE)
##***part040e;
sink()
