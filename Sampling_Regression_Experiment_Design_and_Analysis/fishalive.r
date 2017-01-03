# Impact of different levels of Selenium on the deformity proportions of fish
#
# A fish is a popular pet for young children – yet the survival rate of many of these fish is likely poor.
# What factors seem to influence the survival probabilities of pet fish?
# A large pet store conducted a customer follow-up survey of purchasers of pet fish. A number of
# customers were called and asked about the hardness of the water used for the fish 
# (soft, medium, or hard), where the fish was kept which was then classified into
# cool or hot locations within the living
# dwelling, if they had previous experience with pet fish (yes or no), and if the pet fish was alive six
# months after purchase (yes or no).# 


# Change log
#   2015-07-08 CJS First edition
#
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#
options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Define the logit and expit transform
logit <- function(p){log(p/(1-p))}
expit <- function(logit){1/(1+exp(-logit))}

logit(seq(.1,.9,.1))
expit(logit(seq(.1,.9,.1)))



# Read in the data
sink('fishalive-R-data.txt', split=TRUE)
##***part001b;
# Read in the raw data
fish <- read.csv("fishalive.csv", header=TRUE, as.is=TRUE)

# Set the categories as factors
fish$Softness    <- factor(fish$Softness)
fish$Temperature <- factor(fish$Temperature)
fish$PrevFish    <- factor(fish$PrevFish)

fish$Dead        <- fish$Trials - fish$Alive
fish$palive      <- fish$Alive/ fish$Trials
fish$logitalive <- log( fish$palive/(1-fish$palive))

head(fish)
##***part001e;
sink()


##***partprofileb;
# Create some profile plots
fish$TempPrev   <- factor(paste(fish$Temperature, '-', fish$PrevFish, sep=""))
# Add confidence bounds
fish$plcl <- fish$palive - qnorm(.975)*sqrt(fish$palive*(1-fish$palive)/fish$Trials)
fish$pucl <- fish$palive + qnorm(.975)*sqrt(fish$palive*(1-fish$palive)/fish$Trials)
fish$logitlcl <- fish$logitalive - qnorm(.975)*sqrt(1/(fish$palive*(1-fish$palive)*fish$Trials))
fish$logitucl <- fish$logitalive + qnorm(.975)*sqrt(1/(fish$palive*(1-fish$palive)*fish$Trials))

profile1 <- ggplot(data=fish, aes(x=Softness, y=logitalive, group=TempPrev, 
                                  color=TempPrev, shape=TempPrev, linetype=TempPrev))+
   ggtitle("logit scale")+
  theme(legend.position="top")+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+
  ylab("logit p(alive) with 95% ci")+
  geom_point(size=2,position=position_dodge(width=0.1))+
  geom_line(size=2, position=position_dodge(width=0.1))+
  geom_errorbar(aes(ymin=logitlcl, ymax=logitucl), width=0.1, position=position_dodge(width=0.1))

profile2 <- ggplot(data=fish, aes(x=Softness, y=palive, group=TempPrev, 
                                  color=TempPrev, shape=TempPrev, linetype=TempPrev))+
  ggtitle("prob scale")+
  theme(legend.position="top")+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+
  ylab("p(alive) with 95% ci")+
  geom_point(size=2,position=position_dodge(width=0.1))+
  geom_line(size=2, position=position_dodge(width=0.1))+
  geom_errorbar(aes(ymin=plcl, ymax=pucl), width=0.1, position=position_dodge(width=0.1))
profile <- arrangeGrob(profile2, profile1, nrow=1)
profile
##***partprofilee;

ggsave(plot=profile, file='fishalive-R-profileplot.png', h=4, w=6, units="in", dpi=300)



# We need to use a glm (logisitic regression)

sink('fishalive-R-glmfit.txt', split=TRUE)
##***partglmfitb;
fish.glm <- glm( palive ~ Softness*Temperature*PrevFish, data=fish, weight=Trials,
                     family=binomial(link=logit) )
cat('***** CAUTION - these are incremental and not marginal tests *****\n')
anova(fish.glm, test="Chisq")
# To get the Type III tests, we use the car package
fish.glm2 <- glm( palive ~ Softness*Temperature*PrevFish, data=fish, weight=Trials,
                 family=binomial(link=logit),
                 contrasts=list(Softness=contr.sum, Temperature=contr.sum, PrevFish=contr.sum) )
cat("\n\n***** These are the marginal (Type III) tests *****\n")
car::Anova(fish.glm2, type="III")
##***partglmfite;
sink()

sink('fishalive-R-glmredfit.txt', split=TRUE)
##***partglmredfitb;
# Fit a reduced model
fish.redglm <- glm( palive ~ Softness*PrevFish+Temperature, data=fish, weight=Trials,
                 family=binomial(link=logit) )
cat('***** CAUTION - these are incremental and not marginal tests *****\n')
anova(fish.redglm, test="Chisq")
# To get the Type III tests, we use the car package
fish.redglm2 <- glm( palive ~ Softness*PrevFish+Temperature, data=fish, weight=Trials,
                  family=binomial(link=logit),
                  contrasts=list(Softness=contr.sum, Temperature=contr.sum, PrevFish=contr.sum) )
cat("\n\n***** These are the marginal (Type III) tests *****\n")
car::Anova(fish.redglm2, type="III")
##***partglmredfite;
sink()


# Is the reduced model tenable
anova(fish.glm,  fish.redglm,  test='Chisq')
anova(fish.glm2, fish.redglm2, test='Chisq')

##***partpredictb;
# Make some predictions. 
fish.redglm.STP.lsmo  <- lsmeans::lsmeans(fish.redglm,  ~Softness*Temperature*PrevFish)
fish.redglm.STP.est   <- summary(fish.redglm.STP.lsmo )
# merge with the empirical logit from the data and look at the fit
fish.redglm.STP.est.both <- merge(fish.redglm.STP.est, fish, by=c("Softness","Temperature","PrevFish"))
obsvspred <- ggplot(data=fish.redglm.STP.est.both, aes(x=lsmean, y=logitalive))+
   ggtitle("Observed vs predicted logit survival")+
   ylab("Observed logit survival")+xlab("Predicted logit survival")+
   geom_point()+
   geom_abline(intercept=0, slope=1)
obsvspred
##***partpredicte;
ggsave(plot=obsvspred, file='fishalive-R-obsvspred.png', h=4, w=6, units="in", dpi=300)


#profile plot of final model
fish.redglm.STP.est$TempPrev   <- factor(paste(fish.redglm.STP.est$Temperature, '-', fish.redglm.STP.est$PrevFish, sep=""))

profile.final <- ggplot(data=fish.redglm.STP.est, aes(x=Softness, y=lsmean, group=TempPrev, 
                                  color=TempPrev, shape=TempPrev, linetype=TempPrev))+
  ggtitle("Profile plots of FITTED factor effects - logit scale")+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  ylab("logit p(alive) with 95% ci")+
  geom_point(size=2,position=position_dodge(width=0.1))+
  geom_line(size=2, position=position_dodge(width=0.1))+
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width=0.1, position=position_dodge(width=0.1))
profile.final

ggsave(plot=profile.final, file='fishalive-R-finalprofileplot.png', h=4, w=6, units="in", dpi=300)


# You can verify that the two models (with and without the contrast=sum give the same results)
fish.redglm2.STP.lsmo <- lsmeans::lsmeans(fish.redglm2, ~Softness*Temperature*PrevFish)
summary(fish.redglm2.STP.lsmo)


sink('fishalive-R-oddsratio.txt', split=TRUE)
##***partoddsratiob;
# Estimate log(odds ratio) for combination of softness and previous ownership of fish.
fish.redglm.SP.lsmo  <- lsmeans::lsmeans(fish.redglm,  ~Softness*PrevFish)
fish.redglm.SP.pairs <- summary(pairs(fish.redglm.SP.lsmo ), infer=TRUE)
fish.redglm.SP.pairs$OR     <- exp(fish.redglm.SP.pairs$estimate)
fish.redglm.SP.pairs$OR.lcl <- exp(fish.redglm.SP.pairs$asymp.LCL)
fish.redglm.SP.pairs$OR.ucl <- exp(fish.redglm.SP.pairs$asymp.UCL)

fish.redglm.SP.pairs

singleOR <- fish.redglm.SP.pairs[as.character(fish.redglm.SP.pairs$contrast)=='h , n  - h , y ',]
cat("\n\nHere are the results for one comparison \n")
singleOR[,1:8]
cat(" \n")
singleOR[,c(1,9:11)]
##***partoddsratioe;
sink()


# model assessment
sf.autoplot.glm(fish.redglm)



