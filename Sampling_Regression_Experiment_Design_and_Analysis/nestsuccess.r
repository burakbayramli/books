# Nest success as a function of height from the ground
# 2015-07-05 CJS First Edition


# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the data
sink('nestsuccess-R-001.txt', split=TRUE)
##***part001b;
nests <- read.csv("nestsuccess.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
nests$phat <- nests$Successful/nests$Nests 
nests$elogit <- log( (nests$Successful+.5)/(nests$Nests-nests$Successful+.5))
head(nests)
##***part001e;
sink()

##***partprelimb;
# Preliminary plot of observed probability of success vs height
prelim <- ggplot(data=nests, aes(x=Height, y=phat))+
  ggtitle("Observed p(success) vs height")+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
prelim
##***partprelime;
ggsave(plot=prelim, file='nestsuccess-R-prelim.png', h=4, w=6, units="in", dpi=300)


# Preliminary plot of empirical logit of success vs height
elogitplot <- ggplot(data=nests, aes(x=Height, y=elogit))+
  ggtitle("Empirical logit(success) vs height")+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
elogitplot
ggsave(plot=elogitplot, file='nestsuccess-R-elogit.png', h=4, w=6, units="in", dpi=300)


sink('nestsuccess-R-glmfit.txt', split=TRUE)
##***partglmfitb;
# Fit a logistic regression
nests.fit <- glm(phat ~ Height, data=nests, weight=Nests, family=binomial(link=logit))
anova(nests.fit, test='Chisq')
summary(nests.fit)
confint(nests.fit)
##***partglmfite;
sink()

# Draw the fitted curve
newX <- data.frame(Height=seq(0, max(nests$Height)+2,.1))
nests.pred <- predict(nests.fit, newdata=newX, type="response", se.fit=TRUE)
nests.pred <- cbind(newX, fit=nests.pred$fit, fit.se=nests.pred$se.fit)
nests.pred$lcl <- pmax(0,nests.pred$fit - qnorm(.975)*nests.pred$fit.se )# 95% ci
nests.pred$ucl <- pmin(1,nests.pred$fit + qnorm(.975)*nests.pred$fit.se)
head(nests.pred)

fitplot <- ggplot(data=nests.pred, aes(x=Height, y=fit))+
  ggtitle("Observed p(success) vs height with fitted logistic curve")+
  geom_line()+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  geom_point(data=nests, aes(x=Height, y=phat))
fitplot
ggsave(plot=fitplot, file='nestsuccess-R-fitplot.png', h=4, w=6, units="in", dpi=300)


# diagnostic plots
sf.autoplot.glm(nests.fit)
