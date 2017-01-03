# Can blood mercury be predicted from food blood diets?

# Food blood diets (e.g. number of fish, species of fish, etc) were 
# recorded for a sample of people. Based on estimates of mercury in the 
# food, the mercury in the diet was estimated.  
# A blood sample was also taken from these people 
# and the blood mercury level was also 
# measured.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


# Read in the data
sink('mercury-R-001.txt')
##---part001b;
merc <- read.csv("mercury.csv", header=TRUE)
merc
##---part001e;
sink()

# Plot the raw data
png('mercury-R-002.png')
##---part002b;
plot(merc$intake, merc$blood,
    main='Blood Hg vs. intake Hg',
    ylab='Blood Hg',
    xlab='Intake Hg')
##---part002e;
dev.off()

# Fit the regression line and get the results
png('mercury-R-003.png')
sink('mercury-R-003.txt')
##---part003b;
blood.fit.outliers <- lm(blood ~ intake, data=merc)
summary(blood.fit.outliers)
plot(residuals(blood.fit.outliers))
abline(h=0)
##---part003e;
sink()
dev.off()

# Remove the outliers and refit the line
# Fit the regression line and get the results
png('mercury-R-003a.png')
sink('mercury-R-003a.txt')
##---part003ab;
keep <- abs(residuals(blood.fit.outliers))<100
merc2 <- merc[keep,]
blood.fit <- lm(blood ~ intake, data=merc2)
summary(blood.fit)
plot(merc$intake, merc$blood,
    main='Blood Hg vs. intake Hg',
    sub='Outliers denoted by filled circles',
    ylab='Blood Hg',
    xlab='Intake Hg',
    pch=16+keep)
abline(blood.fit)
##---part003ae;
sink()
dev.off()

# Extract the coefficients and find a confidence interval for them
sink('mercury-R-004.txt')
##---part004b;
confint(blood.fit)
##---part004e;
sink()

# Make predictions at new values of X for the mean and individual
sink('mercury-R-005.txt')
##---part005b;
my.data <- data.frame(intake=c(200,300,400))

cat('Confidence intervals for the MEAN response\n')
my.pred.mean <- predict(blood.fit, newdata=my.data,
    se.fit=TRUE, interval="confidence")
cbind(intake=my.data$intake,
    my.pred.mean$fit, 
    se=my.pred.mean$se.fit)
    
cat('Confidence intervals for the INDIVIDUAL response\n',
    'Note that se for predictions are NOT computed\n')
my.pred.indiv <- predict(blood.fit, newdata=my.data,
    interval="prediction")
cbind(intake=my.data$intake,
    my.pred.indiv)

##---part005e;
sink()


# draw a plot with the fitted lines and the various confidence intervals
png('mercury-R-006.png')
##---part006b;
my.pred.mean<- predict(blood.fit, newdata=merc,
   interval='confidence')
my.pred.indiv <- predict(blood.fit, newdata=merc,
   interval='prediction')

plot(merc$intake, merc$blood,
    main='Blood Hg vs. intake Hg')
abline(blood.fit)  # notice use the object
# We need to sort by the intake values to get nice plots
lines(merc$intake[order(merc$intake)], my.pred.mean[order(merc$intake),"lwr"], lty=2)
lines(merc$intake[order(merc$intake)], my.pred.mean[order(merc$intake),"upr"], lty=2)
lines(merc$intake[order(merc$intake)], my.pred.indiv[order(merc$intake),"lwr"],lty=3)
lines(merc$intake[order(merc$intake)], my.pred.indiv[order(merc$intake),"upr"],lty=3)
##---part006e;
dev.off()

# create a 2x2 plot of residual and other plots
png('mercury-R-010.png')
##---part010b;
layout(matrix(1:4,2,2))
plot(blood.fit)
##---part010e;
dev.off()
















