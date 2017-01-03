# Tuscaloosa Average Yearly Temperatures
# 2015-07-21 CJS added comparsion of slopes in non-parallel model; fix Type III tests
# 2014-11-19 CJS removed Base R graphics; change all ##*** to ##***
# 2014-05-04 CJS first editions

# Consider a time series of annual average temperatures measured at 
# Tuscaloosa, Alabama from 1901 to 2001. 
# It is well known that shifts in temperature can occur whenever the 
# instrument or location or observer or other characteristics of the station change.

# In this case, this corresponds to the years
# 1901-1938 (inclusive); 1940-1956 (inclusive); 1958-1986 (inclusive), and 
# 1989-2000 (inclusive). Note that the years 1939, 1957, and 1987 are NOT 
# used because the average #temperature in these two years is an 
# amalgam of two different recording #conditions

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


library(car)
library(ggplot2)
library(gridExtra)
library(Kendall)
library(lmtest)
library(lsmeans)
library(plyr)

#source("../../schwarz.functions.r")
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")



# Read in the data. Declare Epoch as a factor. Remove data points when location changed in a year
sink('tuscaloosa-R-010.txt', split=TRUE)
##***part010b;
tusctemp <- read.csv("tuscaloosa.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string="")  # here missing values are blanks or null cells
tusctemp <- tusctemp[complete.cases(tusctemp[,c("Year","Epoch","Avg.Temp..C.")]),] # drop the missing values
tusctemp$Epoch <- factor(tusctemp$Epoch)
head(tusctemp)
str(tusctemp)
##***part010e;
sink()

# make an initial plot of the data
# Notice how we specify a different plotting symbol for each Epoch.
##***partprelimplotb;
plotprelim <- ggplot(data=tusctemp, aes(x=Year, y=Avg.Temp..C., 
                      shape=Epoch, color=Epoch))+
  ggtitle("Tuscaloosa Avg Temp (C) over time")+
  xlab("Year")+ylab("Tuscaloosa Avg Temp (C)")+
  geom_point(size=4)
plotprelim
##***partprelimplote;
ggsave(plot=plotprelim, file='tuscaloosa-R-prelimplot.png', h=4, w=6, units="in", dpi=300)


# Look at the data around an epoch change
sink("tuscaloosa-R-partdata.txt", split=TRUE)
tusctemp[ tusctemp$Year %in% 1936:1945,]
sink()


sink('tuscaloosa-R-regfitsep.txt', split=TRUE)
##***partregfitsepb;
# Fit a separate line for each epoch and plot them
d_ply(tusctemp, "Epoch", function(x){
  # fit a separate line for each Epoch
  cat("\n\n***Separate fit for Epoch :", as.character(x$Epoch[1]),"\n")
  fit <- lm( Avg.Temp..C. ~ Year, data=x)
  print(summary(fit))
  print(confint(fit)) # confidence interval on slope
})
##***partregfitsepe;
sink()

prelimplot2 <- plotprelim +
  geom_smooth(method='lm', se=FALSE)
prelimplot2
ggsave(plot=prelimplot2, file="tuscaloosa-R-prelimplot2.png", h=4, w=6, units="in", dpi=300)

sink('tuscaloosa-R-regfitnp.txt', split=TRUE)
##***partregfitnpb;
# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the contrast type and 
# use the Anova() function from the car package
# Be sure that Epoch has been declared as a factor.
tusctemp.fit.np <- lm( Avg.Temp..C. ~ Epoch + Year + Year:Epoch, data=tusctemp,
                       contrasts=list(Epoch="contr.sum"))
Anova(tusctemp.fit.np,type="III")
##***partregfitnpe;
sink()

sink("tuscaloosa-R-regcompslopes.txt", split=TRUE)
##***partregcompslopesb;
tusctemp.fit.np.lsmo <- lsmeans::lstrends(tusctemp.fit.np, ~Epoch, var="Year")
summary(tusctemp.fit.np.lsmo, infer=TRUE)
cld(tusctemp.fit.np.lsmo)
##***partregcompslopese;
sink()





sink('tuscaloosa-R-regfitp.txt', split=TRUE)
##***partregfitpb;
# Fit the regression line with parallel slopes. 
# Because lm() produces type I (increment tests), you need to specify the contrast type and 
# use the Anova() function from the car package
# Be sure that Epoch has been declared as a factor.
tusctemp.fit.p <- lm( Avg.Temp..C. ~ Year + Epoch, data=tusctemp,
                      contrasts=list(Epoch="contr.sum"))
Anova(tusctemp.fit.p, type='III')
##***partregfitpe;
sink()

sink("tuscaloosa-R-fitpieces.txt", split=TRUE)
##***partfitpiecesb;
# Extract the individual parts of the fit using the 
# standard methods. Note that because Epoch is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Epoch effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(tusctemp.fit.p)
coef(tusctemp.fit.p)
sqrt(diag(vcov(tusctemp.fit.p))) # gives the SE
confint(tusctemp.fit.p)
names(summary(tusctemp.fit.p))
summary(tusctemp.fit.p)$r.squared
summary(tusctemp.fit.p)$sigma
##***partfitpiecese;
sink()



sink("tuscaloosa-R-dwtest.txt", split=TRUE)
##***partdwtestb;
# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(tusctemp.fit.p) # from the car package
  dwtest(tusctemp.fit.p) # from the lmtest package
##***partdwteste;
sink()

sink('tuscaloosa-R-lsmeans.txt', split=TRUE)
##***partlsmeansb;
# Estimate the size of the Epoch effects. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
tusctemp.fit.p.lsmo <- lsmeans::lsmeans(tusctemp.fit.p, ~Epoch)
Epochdiff <- pairs(tusctemp.fit.p.lsmo)
summary(Epochdiff, infer=TRUE)
cld(tusctemp.fit.p.lsmo)
##***partlsmeanse;
sink()


# plot the fitted lines to the graphs
# Because there are multiple lines, it is better to make predictions and
# then plot the predictions on the plot. See later.

sink('tuscaloosa-R-predsetup.txt', split=TRUE)
##***partpredsetupb;
# make predictions
# First set up the points where you want predictions
newYears <- rbind( tusctemp[,c("Year","Epoch")],
                   data.frame(Year=2001:2030, Epoch=tusctemp$Epoch[tusctemp$Year==2000]))
newYears[1:5,]
str(newYears)
##***partpredsetupe;
sink()

sink('tuscaloosa-R-predavg.txt', split=TRUE)
##***partpredavgb;
# Predict the AVERAGE temperature in each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(tusctemp.fit.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)
temp <- predict.avg.df[predict.avg.df$Year==2020,]
temp

plotfit <- plotprelim + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit))
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci
##***partpredavge;
sink()

ggsave(plot=plotfit, file='tuscaloosa-R-plotfit.png',            h=4, w=6, units="in", dpi=300)
ggsave(plot=plotfit.avgci, file='tuscaloosa-R-plotpredavg.png',  h=4, w=6, units="in", dpi=300)


sink('tuscaloosa-R-predindiv.txt', split=TRUE)
##***partpredindivb;
# Predict the INDIVIDUAL average temperature. This again needs
# to be interpretted carefully. This gives the potential range
# in the yearly average in a selected year and not the
# range in the individual daily temperatures.
# R does not product the se for individual predictions
predict.indiv <- predict(tusctemp.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
predict.indiv.df    [predict.indiv.df$Year==2020,]
temp <- predict.avg.df[predict.avg.df$Year==2020,]
temp


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci
##***partpredindive;
sink()
ggsave(plot=plotfit.indivci, file='tuscaloosa-R-plotpredindiv.png', h=4, w=6, units="in", dpi=300)




# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended

##***partinvpredb;
plotinvpred <- plotfit.indivci +
  geom_hline(yintercept=19)
plotinvpred
##***partinvprede;

ggsave(plot=plotinvpred, file='tuscaloosa-R-plotinvpred.png', h=4, w=6, units="in", dpi=300)

png('tuscaloosa-R-diagplot.png', h=6, w=6, units="in", res=300)
##***partdiagplotb;
# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- sf.autoplot.lm(tusctemp.fit.p)
grid.newpage()
grid.draw(plotdiag)
##***partdiagplote;
dev.off()

grid.draw(plotdiag)
#ggsave(plot=plotdiag, file='tuscaloosa-R-diagplot.png', h=6, w=6, units="in", dpi=300)





###################################################################################
# Kendall test for trend not really sensible here because of the regime shift
# as the epoch changes.

