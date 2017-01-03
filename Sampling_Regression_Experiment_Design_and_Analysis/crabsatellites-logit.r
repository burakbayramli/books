# What factor affect attraction of males to horseshoe crabs
# Logistic regression with ANCOVA
#

# Change log
#   2015-07-08 CJS First edition
#
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#
options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(GGally)
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
sink('crabsatellites-logit-R-data.txt', split=TRUE)
##***part001b;
# Read in the raw data
crabs <- read.csv("crabsatellites.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)

# Set the categories as factors
crabs$ColorF     <- factor(crabs$Color)
crabs$SpineF     <- factor(crabs$Spine)
crabs$MalesF     <- factor(crabs$Males, levels=c("no","yes"), order=TRUE)
head(crabs)
##***part001e;
sink()

str(crabs)

##***partpairsb;
# create a scatterplot matrix
pairsplot <- ggpairs(crabs, columns=c(1,2,3,5,4)) 
pairsplot
##***partpairse;

png("crabsatellites-logit-R-pairsplot.png", h=4, w=6, units="in", res=300)
pairsplot
dev.off()


# Look at relationship between weight and width in more detail
weightplot <- ggplot(data=crabs, aes(x=Width, y=Weight))+
  ggtitle("Weight vs Width")+
  geom_point()
weightplot
ggsave(plot=weightplot, file="crabsatellites-logit-R-weightplot.png", h=4, w=6, units="in", dpi=300)


# Remove some outliers
outliers <- (crabs$Weight <1500 & crabs$Width >25) |
            (crabs$Width  <21.5) | 
            (crabs$Width  >33)
sum(outliers)
crabs[outliers,]
weightplot + annotate("text", x=crabs$Width[outliers], y=crabs$Weight[outliers], label="X") # check outliers

dim(crabs)
crabs <- crabs[!outliers,]
dim(crabs)

sink("crabsatellites-logit-R-prelimcomp.txt", split=TRUE)
##***partprelimcompb;
# Prelimnary computations of p(satellite males) by weight class and color
report <- ddply(crabs, c("Color","ColorF","WeightClass"), summarize, pmale=mean(Males=="yes"))
report
##***partprelimcompe;
sink()


##***partprelimplotb;
prelimplot <- ggplot(data=report, aes(x=WeightClass, y=pmale, group=ColorF, color=ColorF, linetype=ColorF))+
  ggtitle("P(Male) by weight class and color")+
  geom_point(position=position_dodge(w=0.2))+
  geom_line (position=position_dodge(w=0.2))
##***partprelimplote;

ggsave(plot=prelimplot, file='crabsatellites-logit-R-prelimplot.png', h=4, w=6, units="in", dpi=300)




# We need to use a glm (logisitic regression)

sink('crabsatellites-logit-R-glmfit.txt', split=TRUE)
##***partglmfitb;
crabs.glm <- glm( MalesF ~ ColorF*Weight, data=crabs,
                     family=binomial(link=logit) )
cat('***** CAUTION - these are incremental and not marginal tests *****\n')
anova(crabs.glm, test="Chisq")
# To get the Type III tests, we use the car package
crabs.glm2 <- glm( MalesF ~ ColorF*Weight, data=crabs,
                 family=binomial(link=logit),
                 contrasts=list(ColorF=contr.sum) )
cat("\n\n***** These are the marginal (Type III) tests *****\n")
car::Anova(crabs.glm2, type="III")
##***partglmfite;
sink()


sink('crabsatellites-logit-R-glmredfit.txt', split=TRUE)
##***partglmredfitb;
# Fit a reduced model
crabs.redglm <- glm( MalesF ~ ColorF+Weight, data=crabs,
                 family=binomial(link=logit) )
cat('***** CAUTION - these are incremental and not marginal tests *****\n')
anova(crabs.redglm, test="Chisq")
# To get the Type III tests, we use the car package
crabs.redglm2 <- glm( MalesF ~ ColorF+Weight, data=crabs,
                  family=binomial(link=logit),
                 contrasts=list(ColorF=contr.sum) )
cat("\n\n***** These are the marginal (Type III) tests *****\n")
car::Anova(crabs.redglm2, type="III")
##***partglmredfite;
sink()


# Is the reduced model tenable
anova(crabs.glm,  crabs.redglm,  test='Chisq')
anova(crabs.glm2, crabs.redglm2, test='Chisq')

##***partpredictb;
# Draw a predicted plot
newX <- data.frame(expand.grid(ColorF=unique(crabs$ColorF), Weight=seq(1200,4000,50)))
pred <- predict(crabs.redglm, newdata=newX, type="response", se.fit=TRUE)
# Add the predictions etc to the newx data frame
newX$Pred <- pred$fit
newX$lcl  <- pred$fit - qnorm(.975)*pred$se.fit
newX$ucl  <- pred$fit + qnorm(.975)*pred$se.fit
##***partpredicte;

predictplot <- ggplot(data=newX, aes(x=Weight, y=Pred, group=ColorF, color=ColorF, linetype=ColorF))+
  ggtitle("Predicted probabiity of satellite male")+
  ylab("predicted P(Male) PRESENT")+
  geom_point()+
  geom_line()
predictplot
ggsave(plot=predictplot, file='crabsatellites-logit-R-predictplot.png', h=4, w=6, units="in", dpi=300)


sink('crabsatellites-logit-R-colorcld.txt', split=TRUE)
##***partcolorcldb;
# Compare the intercepts using a compact letter display
color.lsmo <- lsmeans::lsmeans(crabs.redglm, ~ColorF)
summary(color.lsmo)
contrast(color.lsmo, list(c1=c(-1, -1, -1, 3)/3))
##***partcolorclde;
sink()

# model assessment
sf.autoplot.glm(crabs.redglm)



