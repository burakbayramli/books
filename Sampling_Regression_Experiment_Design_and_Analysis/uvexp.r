# Single Factor RCB with sub-sampling
  
# Weight gain of fry after UV exposure.

# An experiment was conducted to measure the weight gain of salmon fry
# after exposure to different combinations of UV A and B radiation. Each 
# tank had 5 fish. Groups of tanks were blocked together in flumes
# with common water flow.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('uvexp-R-001.txt')
##---part001b;
uvexp <- read.csv("uvexp.csv", header=TRUE)
uvexp[1:10,]
##---part001e;
sink()


#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the uvexp for each fish
# Use the summaryBy() function in the doBy package

sink('uvexp-R-020.txt')
##---part020b;
library(doBy)
avg <- summaryBy(WeightGain ~ Block + Trt+ Flume , FUN=mean, data=uvexp)
avg
##---part020e;
sink()

# Plot the data
png(file="uvexp-R-021.png")  # send the plot to a png file
##---part021b;
boxplot(WeightGain.mean ~ Trt, data=avg,  main="Avg weight content for each treatment", 
        sub='Whiskers extend to range of data')
stripchart(WeightGain.mean ~ Trt, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg weight gain for each treatment", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average weight gain')
##---part021e;
dev.off()


# Compute some summary statistics for each group
sink('uvexp-R-022.txt')
##---part022b;
library(doBy)
report <- summaryBy(WeightGain.mean ~ Trt, data=avg, FUN=c(length,mean,sd))
# SE not computed in the usual way because this is not a CRD
report
##---part022e;
sink()




# fit the linear model and get the ANOVA table and test for effects
sink('uvexp-R-030.txt')
##---part030b;
cat("Is the Block variable a factor?",is.factor(avg$Block),"\n")
avg$Block <- as.factor(avg$Block)
cat("Is the Block variable a factor?",is.factor(avg$Block),"\n")

# Trt is usually specified last in model because R gives Type I tests.
# In this case, the design is balanced so it doesn't matter.
result <- lm(WeightGain.mean ~ Block + Trt, data=avg)
anova(result)
##---part030e;
sink()




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

sink('uvexp-R-040.txt')
##---part040b;
lsmeans <- popMeans(result, eff="Trt")
lsmeans
##---part040e;
sink()


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('uvexp-R-045.txt')
png('uvexp-R-045.png')
##---part045b;
library(multcomp)

result.tukey <- glht(result, linfct = mcp(Trt = "Tukey"))
summary(result.tukey)
confint(result.tukey)

result.tukey.cld <- cld(result.tukey)  # joined line plot 
result.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result.tukey)
plot(result.tukey.cld, # main="Multiple comparison results", 
       xlab="Trt",
       ylab="fat", 
       notch=TRUE)
par <- old.par
##---part045e;
sink()
dev.off()


# Check the assumptions of the linear model on the averages
png('uvexp-R-050.png')
##---part050b;
layout(matrix(1:4,2,2))
plot(result)
##---part050e;
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
png(file="uvexp-R-121.png")  # send the plot to a png file
##---part121b;
boxplot(WeightGain ~ Trt, data=uvexp,  main="WeightGain content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(WeightGain ~ Trt, data=uvexp, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg weight gain for each flume", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual weight gains')
##---part121e;
dev.off()



# Fit the random effects model
# Be sure to specify that Flume is a factor
sink('uvexp-R-130.txt')
##---part130b;
cat("Is the Flume variable a factor?",is.factor(uvexp$Flume),"\n")
uvexp$Flume <- as.factor(uvexp$Flume)
cat("Is the fish variable a factor?",is.factor(uvexp$Flume),"\n")
cat("Is the Block variable a factor?",is.factor(uvexp$Block),"\n")
uvexp$Block <- as.factor(uvexp$Block)
cat("Is the Block variable a factor?",is.factor(uvexp$Block),"\n")


library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
uvexp$Flume.id <- interaction(uvexp$Trt,uvexp$Flume)
result2 <- lme( WeightGain ~ Block + Trt, random=~1 | Flume.id, data=uvexp)
cat("*** The following ANOVA table is has a unique label for each fish\n")
anova(result2)

# There is NO easy way in R to specify that Trt*Block is a random effect (the flume)
# and is a random effect. You would hope that 
result2b <- lme( WeightGain ~ Block + Trt, random=~ 1 | Block/Trt , data=uvexp)
cat("\n\n*** The following ANOVA table is WRONG\n")
anova(result2b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 
##---part130e;
sink()


# Get the variance components
sink("uvexp-R-135.txt")
##---part135b;
vc <- VarCorr(result2)
vc
##---part135e;
sink()


# Get the marginal means
sink("uvexp-R-140.txt")
##---part140b;
cat("*** Note that the marginal means cannot be computed in R, and I don't know how to fix this\n")
popMeans(result2, effect='Trt')
##---part140e;
sink()

# Multiple comparison 
sink('uvexp-R-145.txt')
png("uvexp-R-145.png")
##---part145b;
library(multcomp)

result2.tukey <- glht(result2, linfct = mcp(Trt = "Tukey"))
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
         xlab="Trt",
         ylab="WeightGain", 
         notch=TRUE)
par <- old.par
##---part145e;
sink()
dev.off()



#Check the residuals etc
png('uvexp-R-150.png')
##---part150b;
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(result2, resid(., type = "p") ~ fitted(.) | Trt, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(result2, Trt ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(result2, WeightGain ~ fitted(.) | Trt, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(result2, ~ resid(., type = "p") | Trt, abline = c(0, 1))

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
uvexp2 <- uvexp[c(1,3,4,7,8,9,13:nrow(uvexp)),]
table(uvexp2$Trt, uvexp2$Flume)

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the uvexp for each fish
# Use the summaryBy() function in the doBy package

sink('uvexp-R-220.txt')
##---part220b;
library(doBy)
avg <- summaryBy(WeightGain ~ Block + Trt+ Flume, FUN=mean, data=uvexp2)
avg
##---part220e;
sink()

# Plot the data
png(file="uvexp-R-221.png")  # send the plot to a png file
##---part221b;
boxplot(WeightGain.mean ~ Trt, data=avg,  main="Avg fat content for each fish", 
        sub='Whiskers extend to range of data')
stripchart(WeightGain.mean ~ Trt, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg weight gain for each flume", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average fat')
##---part221e;
dev.off()


# Compute some summary statistics for each group
sink('uvexp-R-222.txt')
##---part222b;
library(doBy)
report <- summaryBy(WeightGain.mean ~ Trt, data=avg, FUN=c(length,mean,sd))
# No se is computed because this is a RCB design
report
##---part222e;
sink()





# fit the linear model and get the ANOVA table and test for effects
sink('uvexp-R-230.txt')
##---part230b;
# We usually specify the Trt last in the model because R gives Type I SS
# Because the design is "balanced" when the averages are used, the order doesn't
# matter.
cat("Is the Block variable a factor?",is.factor(avg$Block),"\n")
avg$Block <- as.factor(avg$Block)
cat("Is the Block variable a factor?",is.factor(avg$Block),"\n")

result3 <- lm(WeightGain.mean ~ Block + Trt, data=avg)
anova(result3)
##---part230e;
sink()




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

sink('uvexp-R-240.txt')
lsmeans <- popMeans(result3, eff="Trt")
lsmeans
##---part240e;
sink()


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('uvexp-R-245.txt')
png('uvexp-R-245.png')
##---part245b;
library(multcomp)

result.tukey <- glht(result3, linfct = mcp(Trt = "Tukey"))
summary(result.tukey)
confint(result.tukey)

result.tukey.cld <- cld(result.tukey)  # joined line plot 
result.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result.tukey)
plot(result.tukey.cld, # main="Multiple comparison results", 
       xlab="Trt",
       ylab="fat", 
       notch=TRUE)
par <- old.par
##---part245e;
sink()
dev.off()


# Check the assumptions of the linear model on the averages
png('uvexp-R-250.png')
##---part250b;
layout(matrix(1:4,2,2))
plot(result3)
##---part250e;
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
png(file="uvexp-R-321.png")  # send the plot to a png file
##---part321b;
boxplot(WeightGain ~ Trt, data=uvexp2,  main="WeightGain content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(WeightGain ~ Trt, data=uvexp2, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg weight gain for each flume", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual weight gains')
##---part321e;
dev.off()



# Fit the random effects model
# Be sure to specify that fish is a factor
sink('uvexp-R-330.txt')
##---part330b;
cat("Is the fish variable a factor?",is.factor(uvexp2$Flume),"\n")
uvexp2$Flume <- as.factor(uvexp2$Flume)
cat("Is the fish variable a factor?",is.factor(uvexp2$Flume),"\n")
cat("Is the Block variable a factor?",is.factor(uvexp2$Block),"\n")
uvexp2$Block <- as.factor(uvexp2$Block)
cat("Is the Block variable a factor?",is.factor(uvexp2$Block),"\n")

library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
uvexp2$Flume.id <- interaction(uvexp2$Trt,uvexp2$Flume)
result4 <- lme( WeightGain ~ Block + Trt, random=~1 | Flume.id, data=uvexp2)
anova(result4)

# There is NO easy way in R to specify that block*trt should be a random effect
# and is a random effect. You would hope that 
result4b <- lme( WeightGain ~ Block + Trt, random=~ 1 | Flume/Block , data=uvexp2)
anova(result4b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 
##---part330e;
sink()


# Get the variance components
sink("uvexp-R-335.txt")
##---part335b;
vc <- VarCorr(result4)
vc
##---part335e;
sink()


# Get the marginal means
sink("uvexp-R-340.txt")
##---part340b;
# This doesn't work with LME objects - rats.
popMeans(result4, effect='Trt')
##---part340e;
sink()

# Multiple comparison 
sink('uvexp-R-345.txt')
png("uvexp-R-345.png")
##---part345b;
library(multcomp)

result4.tukey <- glht(result4, linfct = mcp(Trt = "Tukey"))
summary(result2.tukey)
confint(result2.tukey)

result4.tukey.cld <- cld(result4.tukey)  # joined line plot
result4.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result4.tukey)
plot(result4.tukey.cld, main="Multiple comparison results", 
         xlab="Trt",
         ylab="WeightGain", 
         notch=TRUE)
par <- old.par
##---part345e;
sink()
dev.off()



#Check the residuals etc
png('uvexp-R-350.png')
##---part350b;
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(result4, resid(., type = "p") ~ fitted(.) | Trt, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(result4, Trt ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(result4, WeightGain ~ fitted(.) | Trt, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(result4, ~ resid(., type = "p") | Trt, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))

##---part350e;
dev.off()




