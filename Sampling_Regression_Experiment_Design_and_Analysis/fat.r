# Single Factor CRD with sub-sampling
  
# Samples of fish of 4 difference species are selected.
# From each fish of each species, three samples are taken of the muscle
# and the fat level is measured.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('fishfat-R-001.txt')
##---part001b;
fishfat <- read.csv("fat.csv", header=TRUE)
fishfat[1:10,]
##---part001e;
sink()


#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the fishfat for each fish
# Use the summaryBy() function in the doBy package

sink('fishfat-R-020.txt')
##---part020b;
library(doBy)
avg <- summaryBy(Fat ~ Species+ Fish, FUN=mean, data=fishfat)
avg[1:10,]
##---part020e;
sink()

# Plot the data
png(file="fishfat-R-021.png")  # send the plot to a png file
##---part021b;
boxplot(Fat.mean ~ Species, data=avg,  main="Avg fat content for each fish", 
        sub='Whiskers extend to range of data')
stripchart(Fat.mean ~ Species, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg fat for each fish", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average fat')
##---part021e;
dev.off()


# Compute some summary statistics for each group
sink('fishfat-R-022.txt')
##---part022b;
library(doBy)
report <- summaryBy(Fat.mean ~ Species, data=avg, FUN=c(length,mean,sd))
report$Fat.mean.se <- report$Fat.mean.sd/sqrt(report$Fat.mean.length)
report
##---part022e;
sink()

# get the individual confidence intervals
sink('fishfat-R-023.txt')
##---part023b;
ci <- tapply(avg$Fat.mean, avg$Species, FUN=t.test)
# use the sapply() function to extract elements from each member of the list
sapply(ci,"[","conf.int")
##---part023;
sink()



# fit the linear model and get the ANOVA table and test for effects
sink('fishfat-R-030.txt')
##---part030b;
result <- lm(Fat.mean ~ Species, data=avg)
anova(result)
##---part030e;
sink()




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

sink('fishfat-R-040.txt')
##---part040b;
lsmeans <- popMeans(result, eff="Species")
lsmeans
##---part040e;
sink()


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('fishfat-R-045.txt')
png('fishfat-R-045.png')
##---part045b;
library(multcomp)

result.tukey <- glht(result, linfct = mcp(Species = "Tukey"))
summary(result.tukey)
confint(result.tukey)

result.tukey.cld <- cld(result.tukey)  # joined line plot 
result.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result.tukey)
plot(result.tukey.cld, # main="Multiple comparison results", 
       xlab="Species",
       ylab="fat", 
       notch=TRUE)
par <- old.par
##---part045e;
sink()
dev.off()


# Check the assumptions of the linear model on the averages
png('fishfat-R-050.png')
##---part050b;
layout(matrix(1:4,2,2))
plot(result)
##---part050e;
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
png(file="fishfat-R-121.png")  # send the plot to a png file
##---part121b;
boxplot(Fat ~ Species, data=fishfat,  main="Fat content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(Fat ~ Species, data=fishfat, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg fat for each fish", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual fat concentrations')
##---part121e;
dev.off()



# Fit the random effects model
# Be sure to specify that fish is a factor
sink('fishfat-R-130.txt')
##---part130b;
cat("Is the fish variable a factor?",is.factor(fishfat$Fish),"\n")
fishfat$Fish <- as.factor(fishfat$Fish)
cat("Is the fish variable a factor?",is.factor(fishfat$Fish),"\n")

library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
fishfat$Fish.id <- interaction(fishfat$Species,fishfat$Fish)
result2 <- lme( Fat ~ Species, random=~1 | Fish.id, data=fishfat)
cat("*** The following ANOVA table is has a unique label for each fish\n")
anova(result2)

# There is NO easy way in R to specify that fish is nested within species
# and is a random effect. You would hope that 
result2b <- lme( Fat ~ Species, random=~ 1 | Species/Fish , data=fishfat)
cat("\n\n*** The following ANOVA table is WRONG\n")
anova(result2b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 
##---part130e;
sink()


# Get the variance components
sink("fishfat-R-135.txt")
##---part135b;
vc <- VarCorr(result2)
vc
##---part135e;
sink()


# Get the marginal means
sink("fishfat-R-140.txt")
##---part140b;
cat("*** Note that the marginal means cannot be computed in R, and I don't know how to fix this\n")
popMeans(result2, effect='Species')
##---part140e;
sink()

# Multiple comparison 
sink('fishfat-R-145.txt')
png("fishfat-R-145.png")
##---part145b;
library(multcomp)

result2.tukey <- glht(result2, linfct = mcp(Species = "Tukey"))
summary(result2.tukey)
cat("*** Note that the confidence limits below are WRONG, and I don't know how to fix them\n")
confint(result2.tukey)

result2.tukey.cld <- cld(result2.tukey)  # joined line plot
result2.tukey.cld


old.par <- par() 
layout(matrix(1:2,2,1))
plot(result2.tukey)
cat("*** Note that the Joined Letter plot isn't produced, and I don't know how to fix them\n")

plot(result2.tukey.cld, main="Multiple comparison results", 
         xlab="Species",
         ylab="Fat", 
         notch=TRUE)
par <- old.par
##---part145e;
sink()
dev.off()



#Check the residuals etc
png('fishfat-R-150.png')
##---part150b;
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(result2, resid(., type = "p") ~ fitted(.) | Species, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(result2, Species ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(result2, Fat ~ fitted(.) | Species, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(result2, ~ resid(., type = "p") | Species, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))

##---part150e;
dev.off()



#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

# Look what happens when the design is unbalanced?

# Delete some observations
fishfat2 <- fishfat[c(1,3,4,7,8,9,13:nrow(fishfat)),]
table(fishfat2$Species, fishfat2$Fish)

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the fishfat for each fish
# Use the summaryBy() function in the doBy package

sink('fishfat-R-220.txt')
##---part220b;
library(doBy)
avg <- summaryBy(Fat ~ Species+ Fish, FUN=mean, data=fishfat2)
avg[1:10,]
##---part220e;
sink()

# Plot the data
png(file="fishfat-R-221.png")  # send the plot to a png file
##---part221b;
boxplot(Fat.mean ~ Species, data=avg,  main="Avg fat content for each fish", 
        sub='Whiskers extend to range of data')
stripchart(Fat.mean ~ Species, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg fat for each fish", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average fat')
##---part221e;
dev.off()


# Compute some summary statistics for each group
sink('fishfat-R-222.txt')
##---part222b;
library(doBy)
report <- summaryBy(Fat.mean ~ Species, data=avg, FUN=c(length,mean,sd))
report$Fat.mean.se <- report$Fat.mean.sd/sqrt(report$Fat.mean.length)
report
##---part222e;
sink()

# get the individual confidence intervals
sink('fishfat-R-223.txt')
##---part223b;
ci <- tapply(avg$Fat.mean, avg$Species, FUN=t.test)
# use the sapply() function to extract elements from each member of the list
sapply(ci,"[","conf.int")
##---part223;
sink()



# fit the linear model and get the ANOVA table and test for effects
sink('fishfat-R-230.txt')
##---part230b;
result3 <- lm(Fat.mean ~ Species, data=avg)
anova(result3)
##---part230e;
sink()




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

sink('fishfat-R-240.txt')
lsmeans <- popMeans(result3, eff="Species")
lsmeans
##---part240e;
sink()


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('fishfat-R-245.txt')
png('fishfat-R-245.png')
##---part245b;
library(multcomp)

result.tukey <- glht(result3, linfct = mcp(Species = "Tukey"))
summary(result.tukey)
confint(result.tukey)

result.tukey.cld <- cld(result.tukey)  # joined line plot 
result.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result.tukey)
plot(result.tukey.cld, # main="Multiple comparison results", 
       xlab="Species",
       ylab="fat", 
       notch=TRUE)
par <- old.par
##---part245e;
sink()
dev.off()


# Check the assumptions of the linear model on the averages
png('fishfat-R-250.png')
##---part250b;
layout(matrix(1:4,2,2))
plot(result3)
##---part250e;
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
png(file="fishfat-R-321.png")  # send the plot to a png file
##---part321b;
boxplot(Fat ~ Species, data=fishfat2,  main="Fat content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(Fat ~ Species, data=fishfat2, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg fat for each fish", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual fat concentrations')
##---part321e;
dev.off()



# Fit the random effects model
# Be sure to specify that fish is a factor
sink('fishfat-R-330.txt')
##---part330b;
cat("Is the fish variable a factor?",is.factor(fishfat2$Fish),"\n")
fishfat2$Fish <- as.factor(fishfat2$Fish)
cat("Is the fish variable a factor?",is.factor(fishfat2$Fish),"\n")

library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
fishfat2$Fish.id <- interaction(fishfat2$Species,fishfat2$Fish)
result4 <- lme( Fat ~ Species, random=~1 | Fish.id, data=fishfat2)
anova(result4)

# There is NO easy way in R to specify that fish is nested within species
# and is a random effect. You would hope that 
result4b <- lme( Fat ~ Species, random=~ 1 | Species/Fish , data=fishfat2)
anova(result4b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 
##---part330e;
sink()


# Get the variance components
sink("fishfat-R-335.txt")
##---part335b;
vc <- VarCorr(result4)
vc
##---part335e;
sink()


# Get the marginal means
sink("fishfat-R-340.txt")
##---part340b;
popMeans(result4, effect='Species')
##---part340e;
sink()

# Multiple comparison 
sink('fishfat-R-345.txt')
png("fishfat-R-345.png")
##---part345b;
library(multcomp)

result4.tukey <- glht(result4, linfct = mcp(Species = "Tukey"))
summary(result2.tukey)
confint(result2.tukey)

result4.tukey.cld <- cld(result4.tukey)  # joined line plot
result4.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result4.tukey)
plot(result4.tukey.cld, main="Multiple comparison results", 
         xlab="Species",
         ylab="Fat", 
         notch=TRUE)
par <- old.par
##---part345e;
sink()
dev.off()



#Check the residuals etc
png('fishfat-R-350.png')
##---part350b;
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(result4, resid(., type = "p") ~ fitted(.) | Species, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(result4, Species ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(result4, Fat ~ fitted(.) | Species, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(result4, ~ resid(., type = "p") | Species, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))

##---part350e;
dev.off()




