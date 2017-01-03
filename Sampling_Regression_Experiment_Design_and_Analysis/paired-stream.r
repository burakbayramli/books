# Fish density and stream slope.
# 2015-07-21 CJS analyze using ANCOVA
# 2014-04-20 CJS ggplot, etc

# A series of streams were randomly selected. 
# Two sites in each stream were selected and
# the slope of the stream bed and the areal fish 
# density (per sq meter) were measured.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
# Load the necessary libraries
library(car)
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)
library(reshape2)

#source('../../schwarz.functions.r')
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)



# Read in the data
sink('paired-stream-R-001.txt', split=TRUE)
##***part001b;
fish <- read.csv('paired-stream.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
fish$logdensity <- log(fish$density)
fish$StreamF    <- factor(fish$Stream)
head(fish)
##***part001e;
sink()



#*************************************************************************
sink('paired-stream-R-022.txt', split=TRUE)
##***part022b;
# Analyze  using Ancova lm()
# Fit the linear model and get the ANOVA table and test for effects
# Because lm() gives Type I (incremental) tests by default, we need
# to set the contrast matrix.
fish.fit <- lm(logdensity ~ StreamF + slope, data=fish,
             contrasts=list(StreamF="contr.sum"))
##***part022e;
sink()

sink("paired-stream-R-anova.txt", split=TRUE)
##***partanovab;
Anova(fish.fit, type='III')
##***partanovae;
sink()


# Get a plot
newX <- expand.grid(StreamF=unique(fish$StreamF), slope=seq(0,8,.25))
predres <- predict(fish.fit, newdata=newX)
newX <- cbind(newX, pred=predres)
head(newX)
plot1 <- ggplot(data=fish, aes(x=slope, y=logdensity, shape=StreamF, color=StreamF))+
  ggtitle("Predicted fits from ANCOVA model")+
  ylab('log(Density)')+
  geom_point(size=4)+
  geom_line(data=newX, aes(y=pred))
plot1
ggsave(plot=plot1, file='paired-stream-R-plotslope.png', h=4, w=6, units='in', dpi=300)

sink("paired-stream-R-slope.txt", split=TRUE)
##***partslopeb;
# Extract only the slope coefficient
summary(fish.fit)$coefficients[grepl('slope', row.names(summary(fish.fit)$coefficients)),,drop=FALSE]
##***partslopee;
sink()


# Plot of observed vs predicted
fish <- cbind(fish, pred=predict(fish.fit, newdata=fish))
obsvspredplot <- ggplot(data=fish, aes(x=pred, y=logdensity))+
  ggtitle("Observed vs predicted")+
  xlab("Predicted log(density)")+ylab("Observed log(density)")+
  geom_point()+
  geom_abline(slope=1, intercept=0)
obsvspredplot
ggsave(plot=obsvspredplot, file='paired-stream-R-obsvspredplot.png', h=4, w=6, units='in', dpi=300)


# Residual plot


png('paired-stream-R-residplot.png', h=4, w=6, units="in", res=300)
##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
# Groan - the action of the grobArrange() function has changed
plotdiag <-sf.autoplot.lm(fish.fit, which=1, mfrow=c(1,1))
grid.newpage()
grid.draw(plotdiag)
##***partdiage;
#ggsave(plot=plotdiag, file='paired-stream-R-diag.png', h=6, w=6, units="in", dpi=300)
dev.off()
grid.newpage()
grid.draw(plotdiag)




#*************************************************************************
# Remove stream 2 and repeat the analysis
fish2 <- fish[ !fish$Stream==2,]
fish2$StreamF <- factor(fish2$Stream)

# Analyze  using Ancova lm()
# Fit the linear model and get the ANOVA table and test for effects
# Because lm() gives Type I (incremental) tests by default, we need
# to set the contrast matrix.
sink('paired-stream-R-122.txt', split=TRUE)
fish2.fit <- lm(logdensity ~ StreamF + slope, data=fish2,
               contrasts=list(StreamF="contr.sum"))
##***part122e;
sink()

sink("paired-stream-R-anova2.txt", split=TRUE)
##***partanova2b;
cat("After removing stream 2 \n")
Anova(fish2.fit, type='III')
##***partanova2e;
sink()


# Get a plot
newX <- expand.grid(StreamF=unique(fish2$StreamF), slope=seq(0,8,.25))
predres <- predict(fish2.fit, newdata=newX)
newX <- cbind(newX, pred=predres)
head(newX)
plot2 <- ggplot(data=fish2, aes(x=slope, y=logdensity, shape=StreamF, color=StreamF))+
  ggtitle("Predicted fits from ANCOVA model - stream 2 dropped")+
  ylab('log(Density)')+
  geom_point(size=4)+
  geom_line(data=newX, aes(y=pred))
plot2
ggsave(plot=plot2, file='paired-stream-R-plotslope2.png', h=4, w=6, units='in', dpi=300)

sink("paired-stream-R-slope2.txt", split=TRUE)
##***partslope2b;
# Extract only the slope coefficient
summary(fish2.fit)$coefficients[grepl('slope', row.names(summary(fish2.fit)$coefficients)),,drop=FALSE]
##***partslop2ee;
sink()


# Plot of observed vs predicted
fish2 <- cbind(fish2, pred=predict(fish2.fit, newdata=fish2))
obsvspredplot2 <- ggplot(data=fish2, aes(x=pred, y=logdensity))+
  ggtitle("Observed vs predicted - stream 2 dropped")+
  xlab("Predicted log(density)")+ylab("Observed log(density)")+
  geom_point()+
  geom_abline(slope=1, intercept=0)
obsvspredplot2
ggsave(plot=obsvspredplot2, file='paired-stream-R-obsvspredplot2.png', h=4, w=6, units='in', dpi=300)


# Residual plot


png('paired-stream-R-residplot2.png', h=4, w=6, units="in", res=300)
##***partdiag2b;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
# Groan - the action of the grobArrange() function has changed
plotdiag2 <-sf.autoplot.lm(fish2.fit, which=1, mfrow=c(1,1))
grid.newpage()
grid.draw(plotdiag2)
##***partdiag2e;
#ggsave(plot=plotdiag, file='paired-stream-R-diag.png', h=6, w=6, units="in", dpi=300)
dev.off()
grid.newpage()
grid.draw(plotdiag2)


#*********************************************************************
#  Alternate analysis

sink('paired-stream-R-cast.txt', split=TRUE)
##***partcastb;
# Rehape the data from long to wide format
fish.melt <- melt(fish, id.vars=c("Stream","slope.class"), measure.vars=c("slope","logdensity"))
cat('Data frame after melting \n')
head(fish.melt)
fish.wide <- dcast(fish.melt, Stream ~variable+slope.class, value.var=c("value"))
cat('Data frame now in wide format after casting\n')
head(fish.wide)
##***partcaste;
sink()

str(fish.wide)

# Compute difference in slope and log density
fish.wide$diff.slope      <- fish.wide$slope_high      - fish.wide$slope_low
fish.wide$diff.logdensity <- fish.wide$logdensity_high - fish.wide$logdensity_low
sink('paired-stream-R-diff.txt', split=TRUE)
head(fish.wide)
sink()

# plot the differences
diffplot <- ggplot(data=fish.wide, aes(x=diff.slope, y=diff.logdensity))+
  ggtitle("Diff in log density vs diff in slope")+
  geom_point()+
  stat_smooth(data=fish.wide, method='lm', se=FALSE, formula=y ~ 0+x)
diffplot
ggsave(plot=diffplot, file='paired-stream-R-diffplot.png',h=4, w=6, units="in", dpi=300)

##***partwidefitb;
# fit a line through the difference that goes through the origin
fish.wide.fit <- lm( diff.logdensity ~ 0+ diff.slope, data=fish.wide)
##***partwidefite;

sink('paired-stream-R-widefit.txt', split=TRUE)
summary(fish.wide.fit)$coefficients
sink()
