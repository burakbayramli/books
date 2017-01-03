# Impact of development on water quality
# 2014-10-20 CJS Update with ggplot,lsmeans, and lmer package

# Water quality monitoring studies often take the form of incomplete Event 
# designs. For example, the following data represents TSS in water samples 
# taken upstream of a development (the {\it reference} sample), at the 
# development (the{\it mid-stream} sample), or downstream of the development 
# (the {\it ds} sample). Samples are taken during storm Events when water 
# quality may be compromised by the development. Here is a small set of data.
# \footnote{Such a small set of data likely has very poor power to detect 
# anything but very large differences in water quality among  the three 
# Locations. Before conducting such a study, please perform a power analysis 
# to ensure that sufficient samples are taken.}:

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#


options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lmerTest)
library(lsmeans)
library(nlme)


# Read in the data
sink('water-quality-R-001.txt', split=TRUE)
##***part001b;
wq <- read.csv('water-quality.csv', header=TRUE,
         na.strings = ".", as.is=TRUE, strip.white=TRUE)
wq$logTSS <- log(wq$TSS)
wq$Location <- as.factor(wq$Location) # convert to factors
wq$Event    <- as.factor(wq$Event)
wq
##***part001e;
sink()

str(wq)


sink('water-quality-R-010a.txt', split=TRUE)
##***part010ab;
# Note that terms must be in the order Event + Location
# because R does incremental sums of squares and 
# not marginal SS.
result.lm <- lm( logTSS ~ Event + Location, data=wq)
anova(result.lm)
##***part010ae;
sink()


# Estimate the population marginal means.
# Use the lsmeans() function from the lsmeans package
sink('water-quality-R-010b.txt', split=TRUE)
##***part010bb;
result.lm.lsmo <- lsmeans::lsmeans(result.lm, ~Location)
summary(result.lm.lsmo, infer=TRUE, adjust="tukey")
##***part010be;
sink()


# Find all pair-wise comparisons among levels of variable "Location"
sink('water-quality-R-010c.txt', split=TRUE)
##***part010cb;
result.lm.cld <- cld(result.lm.lsmo)
result.lm.cld
pairs(result.lm.lsmo, adjust='tukey')
confint(pairs(result.lm.lsmo, adjust='tukey'))
##***part010ce;
sink()

# Make a plot of the CLD results
source('../../schwarz.functions.r')
plotcld <- sf.cld.plot.bar(result.lm.cld, variable="Location")
plotcld <- plotcld + 
  xlab("Location")+
  ylab("Mean lifetime (with 95% ci")+
  ggtitle("Comparison of mean log(TSS) with cld")
plotcld



#---------------- Combined Inter and Intra-block analysis  using lmer()
#                 where blocks are declared as random effects
sink('water-quality-R-020a.txt', split=TRUE)
##***part020ab;
result.lmer <- lmerTest::lmer( logTSS ~Location + (1|Event), data=wq,
                   na.action=na.omit)
anova(result.lmer, ddf='Kenward-Roger')
##***part020ae;
sink()

# Estimate variance components
VarCorr(result.lmer)


# Estimate the population marginal means.
# Use the lsmeans() function from the lsmeans package
sink('water-quality-R-020b.txt', split=TRUE)
##***part020bb;
result.lmer.lsmo <- lsmeans::lsmeans(result.lmer, ~Location)
summary(result.lmer.lsmo, infer=TRUE, adjust="tukey")
##***part020be;
sink()


# Find all pair-wise comparisons among levels of variable "Location"
sink('water-quality-R-020c.txt', split=TRUE)
##***part020cb;
result.lmer.cld <- cld(result.lmer.lsmo)
result.lmer.cld
pairs(result.lmer.lsmo, adjust='tukey')
confint(pairs(result.lmer.lsmo, adjust='tukey'))
##***part020ce;
sink()

# Make a plot of the CLD results
source('../../schwarz.functions.r')
plotcld <- sf.cld.plot.bar(result.lmer.cld, variable="Location")
plotcld <- plotcld + 
  xlab("Location")+
  ylab("Mean lifetime (with 95% ci")+
  ggtitle("Comparison of mean log(TSS) with cld from lmer()")
plotcld


#---------------- Combined Inter and Intra-block analysis using lme()
#                 where blocks are declared as random effects
#                 This is no longer recommended because lsmeans and other
#                 packages have difficulty in finding the correct df


# We need to use lme for random effect models

result.lme <- lme( logTSS ~Location, data=wq, random=~1 | Event,
               na.action=na.omit)
anova(result.lme)


# Estimate the population marginal means.
# Notice the lsmeans() used infinite df for the comparisons

result.lme.lsmo <- lsmeans::lsmeans(result.lme, ~Location)
summary(result.lme.lsmo, infer=TRUE, adjust="tukey")

result.lme.cld <- cld(result.lme.lsmo)
result.lme.cld
pairs(result.lme.lsmo, adjust='tukey')
confint(pairs(result.lme.lsmo, adjust='tukey'))









