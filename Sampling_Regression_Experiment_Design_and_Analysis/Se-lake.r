# Selenium concentration in fish tissue

# 2014-08-12 CJS First edition

# This example uses (fictitious) but realistic data based on an environmental review
# of a coal mining project. Coal mining often releases Se into the environment
# and this can accumulate in lakes. A set of 9 lakes (labeled $a$ through $i$) was
# selected in the watershed, and in each lake, a sample of fish (ranging from 1 to 34 fish per lake)
# was sampled, and the concentration of Se in the muscle tissue was measured.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(lmerTest)
library(plyr)

source("../../schwarz.functions.r")

sink('Se-lake-R-readdata.txt', split=TRUE)
##***part-readdatab;
fishse <- read.csv('Se-lake.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
fishse$Lake <- factor(fishse$Lake)
fishse[1:10,]
##***part-readdatae;
sink()


#------------------------ Naive Fit ------------------------------
# Fit the naive regression line
fit.naive <- lm( Log_fish_Se ~ Log_Water_Se, data=fishse)

# plot the naive fit on the data point
plot.naive <- ggplot(data=fishse, 
    aes(x=Log_Water_Se, y=Log_fish_Se, shape=Lake))+
    ggtitle("Se Fish vs Se Water with naive fit")+
    xlab("log( [Se water])\nPoints jittered")+ylab("log( [Se fish])")+
    geom_point(size=3, position=position_jitter(width=0.05))+
    scale_shape_manual(values=1:length(unique(fishse$Lake)))+
    geom_abline(intercept=coef(fit.naive)[1], slope=coef(fit.naive)[2])
plot.naive
ggsave(plot=plot.naive, file='Se-lake-R-prelimplot.png', height=4, width=6, units="in")

sink("Se-lake-R-regfit-estimates.txt", split=TRUE)
summary(fit.naive)$coefficients
sink()




#---------------------------- Fit on the Average -----------------
sink("Se-lake-R-findavg.txt", split=TRUE)
##***part-findavgb;
fishse.avg <- ddply(fishse, "Lake", function(x){
    # compute the averages over the lakes
    wat.avg <- mean(x$Log_Water_Se)
    fish.avg<- mean(x$Log_fish_Se)
    res <- c(wat.avg, fish.avg)
    names(res) <- c("Avg.log.water.Se", "Avg.log.fish.Se")
    return(res)
})
fishse.avg
##***part-findavge;
sink()

##***part-fitavgb;
fit.avg <- lm( Avg.log.fish.Se ~ Avg.log.water.Se, data=fishse.avg)

# plot the naive fit on the data point
plot.avg <- ggplot(data=fishse.avg, 
    aes(x=Avg.log.water.Se, y=Avg.log.fish.Se, shape=Lake))+
    ggtitle("Se Fish vs Se Water with fit using averages")+
    xlab("Avg log( [Se water])\nPoints jittered")+ylab("Avg log( [Se fish])")+
    geom_point(size=3, position=position_jitter(width=0.05))+
    scale_shape_manual(values=1:length(unique(fishse.avg$Lake)))+
    geom_abline(intercept=coef(fit.avg)[1], slope=coef(fit.avg)[2])
plot.avg
##***part-fitavge;
ggsave(plot=plot.avg, file='Se-lake-R-prelimplot.avg.png', height=4, width=6, units="in")

sink("Se-lake-R-regfit-avg-estimates.txt", split=TRUE)
summary(fit.avg)$coefficients
sink()



#---------------------------- Fit the individual values using a mixed model -----------------

##***part-fitmixedb;
fit.mixed <- lmerTest::lmer( Log_fish_Se ~ Log_Water_Se + (1|Lake), data=fishse)

# plot the naive fit on the data point
plot.mixed <- ggplot(data=fishse, 
    aes(x=Log_Water_Se, y=Log_fish_Se, shape=Lake))+
    ggtitle("Se Fish vs Se Water with fit using mixed model")+
    xlab("log( [Se water])\nPoints jittered")+ylab("log( [Se fish])")+
    geom_point(size=3, position=position_jitter(width=0.05))+
    scale_shape_manual(values=1:length(unique(fishse$Lake)))+
    geom_abline(intercept=summary(fit.mixed)$coefficients[1,1], 
                slope    =summary(fit.mixed)$coefficients[2,1])
plot.mixed
##***part-fitmixede;
ggsave(plot=plot.mixed, file='Se-lake-R-prelimplot.mixed.png', height=4, width=6, units="in")

sink("Se-lake-R-regfit-mixed-estimates.txt", split=TRUE)
summary(fit.mixed)$coefficients
sink()

# get the variance components
sink("Se-lake-R-regfit-mixed-varcomp.txt", split=TRUE)
##***part-fitmixed-varcorrb;
VarCorr(fit.mixed)
##***part-fitmixed-varcorre;
sink()

# Get predictions at several levels. Predictions
# can be made, but R does not provide the standard errors (groan)
# conditional on the actual lakes in the model
sink("Se-lake-R-regfit-mixed-predict.txt", split=TRUE)
##***part-fitmixed-predictb;
fishse$cond.predict <- predict(fit.mixed, re.form=NULL)
fishse$marg.predict <- predict(fit.mixed, re.form=~0   )

fishse[ fishse$Lake %in% c("c","d"),]
##***part-fitmixed-predicte;
sink()
