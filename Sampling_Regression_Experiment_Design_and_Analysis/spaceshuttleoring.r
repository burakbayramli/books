# Estimate the probability of failure of o-ring on space shuttle as function of launch temperature 
# Logistic regression 

# Actual data after the Challenger disaster 
# 2015-07-05 CJS First Edition

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the data
sink('spaceshuttleoring-R-001.txt', split=TRUE)
##***part001b;
orings <- read.csv("spaceshuttleoring.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
orings$phat <- as.numeric(orings$Outcome=='f')  # 0/1 for outcome 
head(orings)
##***part001e;
sink()

str(orings)

##***partprelimb;
# Preliminary plot of observed probability of success vs height
prelim <- ggplot(data=orings, aes(x=Temperature, y=phat))+
  ggtitle("Observed p(failure) vs temperature")+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
prelim
##***partprelime;
ggsave(plot=prelim, file='spaceshuttleoring-R-prelim.png', h=4, w=6, units="in", dpi=300)




sink('spaceshuttleoring-R-glmfit.txt', split=TRUE)
##***partglmfitb;
# Fit a logistic regression
orings$OutcomeF <- factor(orings$Outcome, levels=c("ok","f"), ordered=TRUE)
orings.fit <- glm(OutcomeF ~ Temperature, data=orings, family=binomial(link=logit))
anova(orings.fit, test='Chisq')
summary(orings.fit)
confint(orings.fit)
##***partglmfite;
sink()


sink('spaceshuttleoring-R-predict.txt', split=TRUE)
##***partpredictb;
# Make some predictions
# To find the confidence intervals for the predictions, predict on the logit scale; find the ci on the logit scale, and then back transform
newX <- data.frame(Temperature=seq(30, max(orings$Temperature)+2,.1))
orings.pred <- predict(orings.fit, newdata=newX, type="link", se.fit=TRUE)
orings.pred <- cbind(newX, fit=orings.pred$fit, fit.se=orings.pred$se.fit)
orings.pred$lcl <- orings.pred$fit - qnorm(.975)*orings.pred$fit.se # 95% ci on logit scale
orings.pred$ucl <- orings.pred$fit + qnorm(.975)*orings.pred$fit.se
# Now conver to the anti-logit scale
orings.pred$fit.p <- 1/(1+exp(-orings.pred$fit))
orings.pred$lcl.p <- 1/(1+exp(-orings.pred$lcl))
orings.pred$ucl.p <- 1/(1+exp(-orings.pred$ucl))

head(orings.pred)
orings.pred[ orings.pred$Temperature == '32',]

fitplot <- ggplot(data=orings.pred, aes(x=Temperature, y=fit.p))+
  ggtitle("Observed p(O-ring failure) vs temperature with fitted logistic curve")+
  geom_line()+
  geom_ribbon(aes(ymin=lcl.p, ymax=ucl.p), alpha=0.2)+
  geom_point(data=orings, aes(x=Temperature, y=phat))
fitplot
##***partpredicte;
sink()
ggsave(plot=fitplot, file='spaceshuttleoring-R-fitplot.png', h=4, w=6, units="in", dpi=300)


# diagnostic plots
sf.autoplot.glm(orings.fit)
