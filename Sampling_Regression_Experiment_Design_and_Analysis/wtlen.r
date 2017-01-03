# Weight length relationship for fish.

# A common technique in fisheries management is to investigate the 
# relationship between weight and lengths of fish.
# This is expected to a non-linear relationship because as fish get longer, 
# they also get wider and thicker. If a fish grew equally in all directions, 
# then the weight of a fish should be proportional to the length**3 (why?). 
# However, fish do not grow equally in all directions, i.e. a doubling of #length is not 
# necessarily associated with a doubling of width or 
# thickness. The pattern of association of weight with length may reveal #information on how fish grow.

# The following example was provided by Randy Zemlak of the British Columbia
# Ministry of Water, Land, and Air Protection.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


# Read in the data
sink('wtlen-R-001.txt')
##---part001b;
wtlen <- read.csv("wtlen.csv", header=TRUE)
wtlen$log.weight <- log(wtlen$weight)
wtlen$log.length <- log(wtlen$length)
wtlen[1:5,]
##---part001e;
sink()

# Plot the raw data and add a loess curve
png('wtlen-R-002.png')
##---part002b;
plot(wtlen$log.length, wtlen$log.weight,
    main='log(weight) vs. log(length) ',
    ylab='log(weight) ',
    xlab='log(length) ')
lines(lowess(wtlen$log.length,wtlen$log.weight))
##---part002e;
dev.off()

# Fit the regression line and get the results
png('wtlen-R-003.png')
sink('wtlen-R-003.txt')
##---part003b;
weight.fit.outliers <- lm(log.weight ~ log.length, data=wtlen)
summary(weight.fit.outliers)
layout(matrix(1:2),2,1)
plot(residuals(weight.fit.outliers))
abline(h=0)
plot(wtlen$log.length, wtlen$log.weight,
    main='log(weight) vs. log(length) ',
    ylab='log(weight) ',
    xlab='log(length) ')
abline(weight.fit.outliers)
##---part003e;
sink()
dev.off()

# Remove the outliers and refit the line
# Fit the regression line and get the results
png('wtlen-R-003a.png')
sink('wtlen-R-003a.txt')
##---part003ab;
keep <- wtlen$log.length <3.7
wtlen2 <- wtlen[keep,]
weight.fit<- lm(log.weight ~ log.length, data=wtlen2)
summary(weight.fit)
layout(matrix(1:2),2,1)
plot(residuals(weight.fit))
abline(h=0)
plot(wtlen2$log.length, wtlen2$log.weight,
    main='log(weight) vs. log(length) 2 large fish removed ',
    ylab='log(weight) ',
    xlab='log(length) ')
abline(weight.fit)
##---part003ae;
sink()
dev.off()

# Extract the coefficients and find a confidence interval for them
sink('wtlen-R-004.txt')
##---part004b;
confint(weight.fit)
##---part004e;
sink()

# Make predictions at new values of X for the mean and individual
sink('wtlen-R-005.txt')
##---part005b;
my.data <- data.frame(log.length=c(2,3,4))

cat('Confidence intervals for the MEAN response\n')
my.pred.mean <- predict(weight.fit, newdata=my.data,
    se.fit=TRUE, interval="confidence")
cbind(log.length=my.data$log.length,
    my.pred.mean$fit, 
    se=my.pred.mean$se.fit)
    
cat('Confidence intervals for the INDIVIDUAL response\n',
    'Note that se for predictions are NOT computed\n')
my.pred.indiv <- predict(weight.fit, newdata=my.data,
    interval="prediction")
cbind(log.length=my.data$log.length,
    my.pred.indiv)

##---part005e;
sink()


# draw a plot with the fitted lines and the various confidence intervals
png('wtlen-R-006.png')
##---part006b;
my.pred.mean<- predict(weight.fit, newdata=wtlen,
   interval='confidence')
my.pred.indiv <- predict(weight.fit, newdata=wtlen,
   interval='prediction')

plot(wtlen$log.length, wtlen$log.weight,
    main='log weight  vs. log length; no outliers')
abline(weight.fit)  # notice use the object
# We need to sort by the length values to get nice plots
lines(wtlen$log.length[order(wtlen$log.length)], my.pred.mean[order(wtlen$log.length),"lwr"], lty=2)
lines(wtlen$log.length[order(wtlen$log.length)], my.pred.mean[order(wtlen$log.length),"upr"], lty=2)
lines(wtlen$log.length[order(wtlen$log.length)], my.pred.indiv[order(wtlen$log.length),"lwr"],lty=3)
lines(wtlen$log.length[order(wtlen$log.length)], my.pred.indiv[order(wtlen$log.length),"upr"],lty=3)
##---part006e;
dev.off()

# create a 2x2 plot of residual and other plots
png('wtlen-R-010.png')
##---part010b;
layout(matrix(1:4,2,2))
plot(weight.fit)
##---part010e;
dev.off()


#****************************************************************************************
# Nonlinear least squares
sink('wtlen-R-020.txt')
png('wtlen-R-020.png')
##---part020b;

wtlen.nls <- nls( weight ~ b0*length**b1, data=wtlen2,  
   start=list(b0=1, b1=3))
summary(wtlen.nls)

plot(wtlen2$length, wtlen2$weight,
    main='weight vs. length fit using nls ',
    ylab='weight ',
    xlab='length ')
points(wtlen2$length[order(wtlen2$length)], predict(wtlen.nls)[order(wtlen2$length)], type="l")
##---part020e;
dev.off()
sink()














