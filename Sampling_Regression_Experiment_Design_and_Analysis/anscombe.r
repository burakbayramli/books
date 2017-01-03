# The Anscombe dataset.
# Anscombe, F. J.  (1973) American Statistician 27, 17-21.
# 2014-03-24 CJS ggplot etc

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(grid)
library(gridExtra)

# Read in the data
sink('anscombe-R-001.txt', split=TRUE)
##---part001b;
ansc <- read.csv("anscombe.csv", header=TRUE, as.is=TRUE,strip.white=TRUE)
ansc
##---part001e;
sink()

# Plot the raw data for all sets of data with the fitted line
##---part002b;
p1 <- ggplot(data=ansc, aes(x=X1, y=Y1))+
  ggtitle("Anscombe Set 1")+
  geom_point(size=2, fill="black")+
  geom_smooth(method='lm')

p2 <- ggplot(data=ansc, aes(x=X2, y=Y2))+
  ggtitle("Anscombe Set 2")+
  geom_point(size=2, fill="black")+
  geom_smooth(method='lm')

p3 <- ggplot(data=ansc, aes(x=X3, y=Y3))+
  ggtitle("Anscombe Set 3")+
  geom_point(size=2, fill="black")+
  geom_smooth(method='lm')

p4 <- ggplot(data=ansc, aes(x=X4, y=Y4))+
  ggtitle("Anscombe Set 4")+
  geom_point(size=2, fill="black")+
  geom_smooth(method='lm')

allplot <- arrangeGrob(p1, p2, p3, p4, nrow=2,
           main='Anscombe Data Sets')
allplot
##---part002e;

ggsave(plot=allplot, file='anscombe-R-002.png')

# Fit the regression lines to each set
set1.fit <- lm(Y1 ~ X1, data=ansc)
set2.fit <- lm(Y2 ~ X2, data=ansc)
set3.fit <- lm(Y3 ~ X3, data=ansc)
set4.fit <- lm(Y4 ~ X4, data=ansc)


# Same plot using base R graphics
layout(matrix(1:4,2,2))
plot(ansc$X1, ansc$Y1,   main='Set 1')
abline(set1.fit)

plot(ansc$X2, ansc$Y2,    main='Set 2')
abline(set2.fit)

plot(ansc$X3, ansc$Y3,     main='Set 3')
abline(set3.fit)

plot(ansc$X4, ansc$Y4,    main='Set 4')
abline(set4.fit)
layout(1)


# Give the results of the fits
sink('anscombe-R-003.txt', split=TRUE)
##---part003b;
summary(set1.fit)
summary(set2.fit)
summary(set3.fit)
summary(set4.fit)
##---part003e;
sink()

##---part003b;
# Look at the residual plots
set1.fit.f <- fortify(set1.fit)
set2.fit.f <- fortify(set2.fit)
set3.fit.f <- fortify(set3.fit)
set4.fit.f <- fortify(set4.fit)

rp1 <-  ggplot(set1.fit.f, aes(.fitted, .resid)) +
    ggtitle("Residual plot - set 1")+
    geom_point()+geom_hline(yintercept=0)

rp2 <-  ggplot(set2.fit.f, aes(.fitted, .resid)) +
    ggtitle("Residual plot - set 2")+
    geom_point()+geom_hline(yintercept=0)

rp3 <-  ggplot(set3.fit.f, aes(.fitted, .resid)) +
    ggtitle("Residual plot - set 3")+
    geom_point()+geom_hline(yintercept=0)

rp4 <-  ggplot(set4.fit.f, aes(.fitted, .resid)) +
    ggtitle("Residual plot - set 4")+
    geom_point()+geom_hline(yintercept=0)
 
allrplot <- arrangeGrob(rp1, rp2, rp3, rp4, nrow=2,
           main='Anscombe Residual Plots')
allrplot
##---part0033;
ggsave(plot=allplot, file='anscombe-R-004.png')


# Look at residual plots using Base R graphics
layout(matrix(1:4,2,2))
plot(fitted(set1.fit),residuals(set1.fit),main='Set 1 Residuals')
plot(fitted(set2.fit),residuals(set2.fit),main='Set 2 Residuals')
plot(fitted(set3.fit),residuals(set3.fit),main='Set 3 Residuals')
plot(fitted(set4.fit),residuals(set4.fit),main='Set 4 Residuals')
layout(1)
