# Single Factor CRD with sub-sampling

# This example is based on work conduced in the laboratory of 
# Prof. Lynn Quarmby at Simon Fraser University 
#  http://www.sfu.ca/mbb/People/Quarmby/. 
# Her research focus is on the the mechanism by which 
# cells shed their cilia (aka flagella) in response to stress working
# with the unicellular alga Chlamydomonas. 

# Microphotographs of the algae are taken, and the 
# length of one or two flagellum of each cell is measured.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('flagella2-R-001.txt')
##---part001b;
lengths <- read.csv("flagella2.csv", header=TRUE)
lengths$Variant.Cell <- paste(lengths$Variant,".",lengths$Cell...,sep="")
lengths[1:10,]
##---part001e;
sink()


#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the lengths for each cell
# We need to stack the data, and then aggregate
sink('flagella2-R-009.txt')
##---part009b;
stack.length <- reshape(lengths, idvar="Variant.Cell",
            varying=list(c("Flagellar.1","Flagellar.2")), 
            times=c("Flagellar.1","Flagellar.2"), timevar="Length",
            v.name="Length", 
            direction="long")
stack.length[1:10,]
##---part009e;
sink()


sink('flagella2-R-009b.txt')
##---part009bb;
avg <- aggregate(Length ~ Variant+ Cell..., FUN=mean, data=stack.length)
avg[1:10,]
##---part009be;
sink()

# Plot the data
png(file="flagella2-R-010.png")  # send the plot to a png file
##---part010b;
boxplot(Length ~ Variant, data=avg,  ain="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data')
stripchart(Length ~ Variant, data=avg, add=TRUE, 
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')
##---part010e;
dev.off()

# Get rid of outliers
png(file="flagella2-R-011.png")  # send the plot to a png file
##---part011b;
avg <- avg[avg$Length > 4.5,] 
boxplot(Length ~ Variant, data=avg,  ain="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data')
stripchart(Length ~ Variant, data=avg, add=TRUE, 
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')
##---part011e;
dev.off()

# Compute some summary statistics for each group
sink('flagella2-R-012.txt')
##---part012b;
library(doBy)
report <- summaryBy(Length ~ Variant, data=avg, FUN=c(length,mean,sd))
report$Length.se <- report$Length.sd/sqrt(report$Length.length)
report
##---part012e;
sink()

# get the individual confidence intervals
sink('flagella2-R-013.txt')
##---part013b;
ci <- tapply(avg$Length, avg$Variant, FUN=t.test)
# use the sapply() function to extract elements from each member of the list
sapply(ci,"[","conf.int")
##---part013;
sink()

# fit the linear model and get the ANOVA table and test for effects
sink('flagella2-R-014.txt')
##---part014b;
result <- aov(Length ~ Variant, data=avg)
anova(result)
##---part014e;
sink()


# Check the assumptions of the ANOVA model
png('flagella2-R-015.png')
##---part015b;
layout(matrix(1:4,2,2))
plot(result)
##---part015e;
dev.off()

# Estimate the marginal means.
# This is a real pain in R as you need to first get a list
# of the factor combinations that you want (the unique() function) 
# and create an appropriate data.frame (expand.grid() function.)

sink('flagella2-R-016.txt')
##---part016b;
pred.Variant <- expand.grid( Variant=unique(avg$Variant))
lsmeans <- predict(result, newdata=pred.Variant, se=TRUE)
lsmeans
cbind(pred.Variant, lsmeans$fit, lsmeans$se)

# LSmeans after a lm() fit
library(doBy)
result.lm <- lm(Length ~ Variant, data=avg)
lsmeans(result.lm, eff="Variant")
##---part016e;
sink()


# Now for a multiple comparison procedures

sink('flagella2-R-017.txt')
png('flagella2-R-017.png')
##---part017b;
mcp <- TukeyHSD(result, ordered=TRUE) # ordered sorts means
mcp
plot(mcp)
abline(v=0, lty=2)
##---part017e;
sink()
dev.off()



# The multiple comparison package has lots of good routines.
# specify all pair-wise comparisons among levels of variable "tension"
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('flagella2-R-018.txt')
png('flagella2-R-018.png')
##---part018b;
library(multcomp)

result.tukey <- glht(result, linfct = mcp(Variant = "Tukey"))
result.tukey.cld <- cld(result.tukey)  # joined line plot

# create the display 
result.tukey.cld
old.par <- par( mai=c(1,1,1.25,1)) # set top margin bigger
plot(result.tukey.cld, # main="Multiple comparison results", 
     xlab="Variant",
     ylab="Length", 
     notch=TRUE)
##---part018e;
sink()
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation

# We start by stacking the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
# on the rehape() function

sink('flagella2-R-030.txt')
##---part030b;
stack.length <- reshape(lengths, idvar="Variant.Cell",
            varying=list(c("Flagellar.1","Flagellar.2")), 
            times=c("Flagellar.1","Flagellar.2"), timevar="Length",
            v.name="Length", 
            direction="long")
stack.length[1:10,]
 # get rid of outliers
stack.length <- stack.length[!(stack.length$Length<10 & 
                             stack.length$Variant=="A"),]
stripchart(Length ~ Variant, data=stack.length)

##---part030e;
sink()

# Fit the random effects model
sink('flagella2-R-031.txt')
##---part031b;
library(lme4)
result2 <- lmer( Length ~ Variant + (1|Variant.Cell), data=stack.length)
summary(result2)
anova(result2) # note no p-value given
##---part031e;
sink()

# Get the marginal means
# There is no easy wasy to do this using lmer()
sink("flagella2-R-032.txt")
##---part032b;
pred.Variant <- expand.grid( Variant=unique(avg$Variant), Length=0)
mm <- model.matrix(terms(result2),pred.Variant)
means <- mm %*% fixef(result2)
means.vcv <- mm %*% tcrossprod(vcov(result2),mm)
means.se <- sqrt(diag(mm %*% tcrossprod(vcov(result2),mm)))
cbind(pred.Variant, means, means.se)
##---part032e;
sink()

# Multiple comparison 
sink('flagella2-R-035.txt')
png("flagella2-R-035.png")
##---part035b;
library(multcomp)

result2.tukey <- glht(result2, linfct = mcp(Variant = "Tukey"))
result2.tukey.cld <- cld(result2.tukey)  # joined line plot

# create the display 
result2.tukey.cld
old.par <- par( mai=c(1,1,1.25,1)) # set top margin bigger
plot(result2.tukey.cld, # main="Multiple comparison results", 
     xlab="Variant",
     ylab="Length", 
     notch=TRUE)
##---part035e;
sink()
dev.off()







