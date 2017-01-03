# Estimating the catch via a creel survey
# 2015-02-21 CJS update split=TRUE; ##--- changed to ##***

# For management purposes, it is important to 
# estimate the total catch by recreational fishers.
# There is no central reporting station, and 
# surveys are often used to estimate the total catch.

# An access survey was conducted to estimate the total
# catch at a lake in British Columbia. Fortunately, 
# access to the lake takes place at a single landing site
# and most anglers use boats in the fishery. 
# An observer was stationed at the landing site, but 
# because of time constraints, could only interview a
# portion of the angling parties returning, but was 
# able to get a total count of the number of fishing 
# parties (boats) on that day. A total of 168 boats 
# arrived at the landing during the day,
# of which 30 were sampled. The decision to sample 
# an angler party was made using a random number 
# table as the parties returned. 

# The objectives are to estimate the total 
# number of anglers and their catch and to estimate 
# the proportion of boat trips that had sufficient 
# life-jackets for the members on the trip. 

# Population size is 168 boats.

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('creel-R-001.txt', split=TRUE)
##***part001b;
creel <- read.csv("creel.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
creel[1:12,]
##***part001e;
sink()


#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
 
# Find the mean, sd, and se of the Anglers and Catch per boat.

sink('creel-R-002.txt', split=TRUE)
##***part002b;
mean.Anglers <- mean(creel$Anglers)
sd.Anglers   <- sd(creel$Anglers)
se.mean.Anglers <- sd.Anglers/sqrt(length(creel$Anglers))
cat("Est Mean Anglers per boat is ", mean.Anglers,
    ";\n    sd of Anglers per boat is ", sd.Anglers,
    ";\n    se of mean(Anglers per boat) is ", se.mean.Anglers,"\n\n")

mean.Catch <- mean(creel$Catch)
sd.Catch   <- sd(creel$Catch)
se.mean.Catch <- sd.Catch/sqrt(length(creel$Catch))
cat("Est Mean Catch per boat is ", mean.Catch,
    ";\n    sd of Catch per boat is ", sd.Catch,
    ";\n    se of mean(Catch per boat) is ", se.mean.Catch,"\n")
##***part002e;
sink()
 
 
# Use the t.test function to do the same but we need to compute the se
# based on the statistic and the estimate
sink('creel-R-003.txt', split=TRUE)
##***part003b;
t.test.Anglers <- t.test(creel$Anglers)
names(t.test.Anglers)
t.test.Anglers$se.mean <- t.test.Anglers$estimate / t.test.Anglers$statistic
cat("Est Mean Anglers per boat is ", t.test.Anglers$estimate,
    ";\n    se of mean(Anglers per boat) is ", t.test.Anglers$se.mean,"\n\n")

t.test.Catch <- t.test(creel$Catch)
names(t.test.Catch)
t.test.Catch$se.mean <- t.test.Catch$estimate / t.test.Catch$statistic
cat("Est Mean Catcj per boat is ", t.test.Catch$estimate,
    ";\n    se of mean(Catch per boat) is ", t.test.Catch$se.mean,"\n\n")
##***part003e;
sink()


# Expand the estimates of the mean and se by the expansion factor 

sink('creel-R-004.txt', split=TRUE)
##***part004b;
TotalBoats <- 168
cat('*** Total boats arriving today is:', TotalBoats, '\n')

Total.Anglers <- mean.Anglers * TotalBoats
se.Total.Anglers <- se.mean.Anglers * TotalBoats
cat("Est Total Anglers  is ", Total.Anglers,
    ";\n    se of Total(Anglers) is ", se.Total.Anglers,"\n\n")

Total.Catch <- mean.Catch * TotalBoats
se.Total.Catch <- se.mean.Catch * TotalBoats
cat("Est Total Catch  ", Total.Catch,
    ";\n    se of total(Catch ) is ", se.Total.Catch,"\n")
##***part004e;
sink()


# We can do this using the t.test. results
t.test.Anglers$Total   <- t.test.Anglers$estimate * TotalBoats
t.test.Anglers$se.Total<- t.test.Anglers$se.mean  * TotalBoats
cat("Est Total Anglers  is ", t.test.Anglers$Total,
    ";\n    se of Total(Anglers) is ", t.test.Anglers$se.Total,"\n\n")

t.test.Catch$Total     <- t.test.Catch$estimate   * TotalBoats
t.test.Catch$se.Total  <- t.test.Catch$se.mean    * TotalBoats
cat("Est Total Catch  ", t.test.Catch$Total,
    ";\n    se of total(Catch ) is ", t.test.Catch$se.Total,"\n")


# Now for the analysis of the LifeJacket variable.

sink('creel-R-005.txt', split=TRUE)
##***part005b;
count.Jackets <- table(creel$Suff.Jackets)
prop.Jackets <- count.Jackets / sum(count.Jackets)
se.prop.Jackets <- sqrt(prop.Jackets * (1-prop.Jackets) / sum(count.Jackets))

cat('Summary of Suff.Jackets variable',
    names(count.Jackets),count.Jackets,
    ';\n with proportions ', prop.Jackets,
    ';\n and se', se.prop.Jackets,'\n\n')
##***part005e;
sink()



# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.
# This is likely overkill for simple random samples and simple
# means and totals

##***part010b;
library(survey)
##***part010e;

sink('creel-R-011.txt', split=TRUE)
##***part011b;
creel$TotalBoats <- 168
creel[1:5,]
##***part011e;
sink()

sink('creel-R-012.txt', split=TRUE)
##***part012b;
creel.design <- svydesign(data=creel, 
      ids=~1, # no clusters
      fpc=~TotalBoats)  # boats in the day
print(creel.design)
##***part012e;
sink()


sink('creel-R-013.txt', split=TRUE)
##***part013b;      
# Estimate the mean anglers and catch/boat
est.means <- svymean(~Anglers+Catch+Suff.Jackets, creel.design)
est.means.ci <- confint(est.means)
est.means
est.means.ci

# est.means and est.means.ci are not matrices and so require
# some additional work to make a nice report.
report.means <- cbind( data.frame(mean=est.means),data.frame(est.means.ci))
report.means
##***part013e;
sink()


sink('creel-R-014.txt', split=TRUE)
##***part014b;
# Estimate the totals anglers and catch over the day
est.totals    <- svytotal(~Anglers+Catch, creel.design)
est.totals.ci <- confint(est.totals)
est.totals
est.totals.ci
report.totals <- cbind( data.frame(mean=est.totals),data.frame(est.totals.ci))
report.totals
##***part014e;
sink()
