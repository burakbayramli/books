  
$ Monitoring fry density over time.

# Salmon fry densities were monitored at 4 different rivers. Up to three 
# sites were selected from each river. Fry densities were measured in a 
# number of years. Not all sites were monitored in all years.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('fry-R-001.txt')
##---part001b;
fry.wide <- read.csv("fry.csv", header=TRUE, na.strings=".")
cat("*** Data in wide format\n")
fry.wide
# We need to stack the data
fry <- reshape(fry.wide, varying=c('X2000','X2001','X2002','X2003','X2004'),
       v.names='Density',
       times=2000:2004, timevar='Year',
       direction="long")
# Drop missing values
fry <- fry[!is.na(fry[,"Density"]),]
fry <- cbind(fry,logDensity=log(fry[,"Density"]))
cat("\n\n*** Data in long (stacked) format\n")
fry[1:10,]
##---part001e;
sink()


# Get a preliminary plot of the data
png('fry-R-005.png')
##---part005b;
plot(fry[,"Year"],fry[,"Density"],
   main='Fry density over time', xlab='Year',ylab='Fry Density')
temp <- lapply(split(fry,list(Location=fry[,"Location"],Site=fry[,"Site"])),
    function(x){
    	lines(x$Year,x$Density)})
##---part005e;
dev.off()

# Get a preliminary plot of the data
png('fry-R-005b.png')
##---part005bb;
plot(fry[,"Year"],fry[,"logDensity"],
   main='log(Fry density) over time', xlab='Year',ylab='log(Fry Density)')
temp <- lapply(split(fry,list(Location=fry[,"Location"],Site=fry[,"Site"])),
    function(x){
    	lines(x$Year,x$logDensity)})
##---part005be;
dev.off()




#--------------------------------------------------
# Method 1. Compute the averages 

# Find the average of the fry density for each Site Location
# Use the summaryBy() function in the doBy package

sink('fry-R-020.txt')
##---part020b;
library(doBy)
avg <- summaryBy(logDensity ~ Location + Year , FUN=mean, data=fry)
avg
##---part020e;
sink()

# Plot the data
png(file="fry-R-021.png")  # send the plot to a png file
##---part021b;
boxplot(logDensity.mean ~ Year, data=avg,  main="Avg log(density) in each year", 
        sub='Whiskers extend to range of data')
stripchart(logDensity.mean ~ Year, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg log(density) for each year", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average log(Density)')
##---part021e;
dev.off()


# Compute some summary statistics for each group
sink('fry-R-022.txt')
##---part022b;
library(doBy)
report <- summaryBy(logDensity.mean ~ Year, data=avg, FUN=c(length,mean,sd))
# SE not computed in the usual way because this is not a CRD
report
##---part022e;
sink()




# fit the linear model and get the ANOVA table and test for effects
sink('fry-R-030.txt')
##---part030b;
cat("Is the Location variable a factor?",is.factor(avg$Location),"\n")
avg$Location <- as.factor(avg$Location)
cat("Is the Location variable a factor?",is.factor(avg$Location),"\n")
cat("Is the Year variable a factor?",is.factor(avg$Year),"\n")
avg$Year <- as.factor(avg$Year)
cat("Is the Year variable a factor?",is.factor(avg$Year),"\n")

# Year is usually specified last in model because R gives Type I tests.
# In this case, the design is balanced so it doesn't matter.
result <- lm(logDensity.mean ~ Location + Year, data=avg)
anova(result)
##---part030e;
sink()




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

sink('fry-R-040.txt')
##---part040b;
lsmeans <- popMeans(result, eff="Year")
lsmeans
##---part040e;
sink()


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
sink('fry-R-045.txt')
png('fry-R-045.png')
##---part045b;
library(multcomp)

result.tukey <- glht(result, linfct = mcp(Year = "Tukey"))
summary(result.tukey)
confint(result.tukey)

result.tukey.cld <- cld(result.tukey)  # joined line plot 
result.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(result.tukey)
plot(result.tukey.cld, # main="Multiple comparison results", 
       xlab="Year",
       ylab="fat", 
       notch=TRUE)
par <- old.par
##---part045e;
sink()
dev.off()


# Check the assumptions of the linear model on the averages
png('fry-R-050.png')
##---part050b;
layout(matrix(1:4,2,2))
plot(result)
##---part050e;
dev.off()



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
png(file="fry-R-121.png")  # send the plot to a png file
##---part121b;
boxplot(logDensity ~ Year, data=fry,  main="logDensity for each year-location-site", 
       sub='Whiskers extend to range of data')
stripchart(logDensity ~ Year, data=fry, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="logDensity over time", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='logDensity')
##---part121e;
dev.off()



# Fit the random effects model
# Be sure to specify that Flume is a factor
sink('fry-R-130.txt')
##---part130b;
cat("Is the Year variable a factor?",is.factor(fry$Year),"\n")
fry$Year <- as.factor(fry$Year)
cat("Is the Year variable a factor?",is.factor(fry$Year),"\n")
cat("Is the Location variable a factor?",is.factor(fry$Location),"\n")
fry$Location <- as.factor(fry$Location)
cat("Is the Location variable a factor?",is.factor(fry$Location),"\n")


library(nlme)
# Create a unique Site-Location id for each combination of Site and Location

fry$Site.id <- interaction(fry$Location,fry$Site)
result2 <- lme( logDensity ~ Location + Year, random=~1 | Site.id, data=fry)
cat("*** The following ANOVA table is has a unique label for each fish\n")
anova(result2)

# There is NO easy way in R to specify that Year*Location is a random effect (the flume)
# and is a random effect. You would hope that 
result2b <- lme( logDensity ~ Location + Year, random=~ 1 | Location/Site , data=fry)
cat("\n\n*** The following ANOVA table is WRONG\n")
anova(result2b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 
##---part130e;
sink()


# Get the variance components
sink("fry-R-135.txt")
##---part135b;
vc <- VarCorr(result2)
vc
##---part135e;
sink()


# Get the marginal means
sink("fry-R-140.txt")
##---part140b;
cat("*** Note that the marginal means cannot be computed in R, and I don't know how to fix this\n")
popMeans(result2, effect='Year')
##---part140e;
sink()

# Multiple comparison 
sink('fry-R-145.txt')
png("fry-R-145.png")
##---part145b;
library(multcomp)

result2.tukey <- glht(result2, linfct = mcp(Year = "Tukey"))
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
         xlab="Year",
         ylab="logDensity", 
         notch=TRUE)
par <- old.par
##---part145e;
sink()
dev.off()



#Check the residuals etc
png('fry-R-150.png')
##---part150b;
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(result2, resid(., type = "p") ~ fitted(.) | Year, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(result2, Year ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(result2, logDensity ~ fitted(.) | Year, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(result2, ~ resid(., type = "p") | Year, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))

##---part150e;
dev.off()



# Do the power analysis

sink('fry-R-160.txt')
##---part160b;
group.means <- c(0, 0.22)
power <- power.anova.test(groups=length(group.means), 
         within.var=0.62**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##---part160e;
sink()




