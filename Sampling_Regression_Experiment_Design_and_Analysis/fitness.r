# Estimate the probability of being a female given other attributes
# This is the fitness.jmp data file that ships with JMP.
# Linneruds Fitness data, fitting oxygen uptake to exercise and other variables. 
# The original is in Rawlings (1988), but certain values of MaxPulse and RunPulse were 
# changed for illustration. Names and Sex columns were contrived for illustration.

# Logistic regression 

# 2015-07-05 CJS First Edition

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(GGally)
library(ggplot2)
library(gridExtra)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Read in the data
sink('fitness-R-001.txt', split=TRUE)
##***part001b;
fitness <- read.csv("fitness.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
fitness$phat <- as.numeric(fitness$Sex=='F')  # 0/1 for Sex 
head(fitness)
##***part001e;
sink()

str(fitness)

##***partprelimb;
# Preliminary plot of observed probability of success vs height
prelim1 <- ggplot(data=fitness, aes(x=Age, y=phat))+
  ggtitle("Observed p(female) vs age")+ylim(c(0,1))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

prelim2 <- ggplot(data=fitness, aes(x=Weight, y=phat))+
  ggtitle("Observed p(female) vs Weight")+ylim(c(0,1))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

prelim3 <- ggplot(data=fitness, aes(x=Oxy, y=phat))+
  ggtitle("Observed p(female) vs Oxygen Consumption")+ylim(c(0,1))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

prelim4 <- ggplot(data=fitness, aes(x=Runtime, y=phat))+
  ggtitle("Observed p(female) vs runtime")+ylim(c(0,1))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

prelim <- arrangeGrob( prelim1, prelim2, prelim3, prelim4, nrow=2)
prelim
##***partprelime;
ggsave(plot=prelim, file='fitness-R-prelim.png', h=4, w=6, units="in", dpi=300)


##***partprelim2b;
# Preliminary plot of observed probability of success vs height
prelim1 <- ggplot(data=fitness, aes(x=Sex, y=Age))+
  ggtitle("Age vs Sex")+
  geom_jitter(position=position_jitter(width=0.2))+
  geom_boxplot(alpha=0.2, notch=TRUE)

prelim2 <- ggplot(data=fitness, aes(y=Weight, x=Sex))+
  ggtitle("Weight vs sex")+
  geom_jitter(position=position_jitter(width=0.2))+
  geom_boxplot(alpha=0.2, notch=TRUE)

prelim3 <- ggplot(data=fitness, aes(y=Oxy, x=Sex))+
  ggtitle("Oxygen Consumption vs Sex")+
  geom_jitter(position=position_jitter(width=0.2))+
  geom_boxplot(alpha=0.2, notch=TRUE)

prelim4 <- ggplot(data=fitness, aes(y=Runtime, x=Sex))+
  ggtitle("Runtime vs sex")+
  geom_jitter(position=position_jitter(width=0.2))+
  geom_boxplot(alpha=0.2, notch=TRUE)

prelim <- arrangeGrob( prelim1, prelim2, prelim3, prelim4, nrow=2)
prelim
##***partprelim2e;
ggsave(plot=prelim, file='fitness-R-prelim2.png', h=4, w=6, units="in", dpi=300)


prelim3 <- ggpairs(data=fitness[,c('Age',"Weight","Oxy","Runtime")])
prelim3
# bother that ggpairs does not create an actual ggplot object
png(file='fitness-R-prelim3.png',h=5, w=6, units="in", res=300)
prelim3
dev.off()


sink('fitness-R-glmfit.txt', split=TRUE)
##***partglmfitb;
# Fit a logistic regression
fitness$SexF <- factor(fitness$Sex, levels=c("M","F"), ordered=TRUE)
fitness.fit <- glm(SexF ~ Age+Weight+Oxy+Runtime, data=fitness, family=binomial(link=logit))
anova(fitness.fit, test='Chisq')
drop1(fitness.fit, test='Chisq')
summary(fitness.fit)
confint(fitness.fit)
##***partglmfite;
sink()

sink('fitness-R-glmdrop.txt', split=TRUE)
##***partglmdropb;
# Can you drop Age and RUntime? We need to fit a reduced model and then do the comparison
fitness.fit.red <- glm(SexF ~ Weight+Oxy, data=fitness, family=binomial(link=logit))
anova(fitness.fit, fitness.fit.red, test='Chisq')
##***partglmdrope;
sink()

sink("fitness-R-glmred.txt", split=TRUE)
##***partglmredb;
# Fit the simpler model
fitness.fit.red <- glm(SexF ~ Weight+Oxy, data=fitness, family=binomial(link=logit))
drop1(fitness.fit.red, test="Chisq")
summary(fitness.fit.red)
confint(fitness.fit.red)
##***partglmrede;
sink()



# diagnostic plots
sf.autoplot.glm(fitness.fit)


resid.red <- sf.autoplot.glm(fitness.fit.red)
resid.red
png(file='fitness-R-residred.png',h=5, w=6, units="in", res=300)
resid.red
dev.off()

#ggsave(resid.red, file='fitness-R-residred.png', h=6, w=6, units="in", dpi=300) # not work - groan


# Illustrating the problem of complete separation
zz <- file("fitness-R-glmrs.txt", open="wt")
sink(zz)
sink(zz, type="message")
##***partglmrsb;
# Try and fit a response surface using the Weight and Oxygen
fitness.fit.rs <- glm(SexF ~ Weight + Oxy + Weight*Weight + Oxy*Oxy + Weight*Oxy, data=fitness, family=binomial(link=logit))
##***partglmrse;
sink(type="message")
sink()
