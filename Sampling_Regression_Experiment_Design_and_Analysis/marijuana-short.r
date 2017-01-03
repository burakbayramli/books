# A study was conducted where Student.usages at a college were asked about their 
# personal use of marijuana and if their parents used alcohol and/or marijuana.
# The following data is a collapsed version of the table that appears in the report:
#  Marijuana Use in College, Youth and Society, 1979, 323-334

# 2015-07-05 CJS First Edition

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(epitools)
library(ggplot2)
library(lsmeans)
library(plyr)

# Read in the data
sink('marijuana-short-R-001.txt', split=TRUE)
##***part001b;
mj <- read.csv("marijuana-short.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
mj$Parental.usage    <- factor(mj$Parental.usage)
mj$Student.usage     <- factor(mj$Student.usage)
mj
##***part001e;
sink()


# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('marijuana-short-R-015.txt', split=TRUE)
##***part015b;
cat("Contingency table of student usage by parental usage\n")
table.mj <- xtabs(Count ~Parental.usage + Student.usage, data=mj)
table.mj
cat("Percentage of student usage by parental usage\n")
round(prop.table(table.mj,margin=1)*100,0)
##***part015e;
sink()


# Compute the proportions and se/ci for each proportion for each type of data
sink('marijuana-short-R-002.txt', split=TRUE)
##***part002b;
report <- ddply(mj, "Parental.usage", function(x){
  # Compute the proportions etc for each Student.usage of Count
  x$prop <- x$Count/ sum(x$Count)
  x$se   <- sqrt(x$prop*(1-x$prop)/x$Count)
  x$lcl  <- x$prop - 2*x$se
  x$ucl  <- x$prop + 2*x$se
  return(x)
})
report
##***part002e;
sink()



##***part004b;
segbar <- ggplot(data=report, aes(x=Parental.usage, y=prop, fill=Student.usage ))+
  ggtitle("Stacked bar chart comparing proportions of usage by Parental.usage usage")+
  xlab("Parental usage")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='marijuana-short-R-segbar.png', h=4, w=6, units="in", dpi=300)


# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=Student.usage, y=prop, fill=Parental.usage))+
  ggtitle("Comparing proportions of usage from two Student.usages")+
  ylab("Proportion in each canopy class (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="marijuana-short-R-barplot.png", h=4, w=6, units="in", dpi=300)


# There is no builtin mosaic plot in ggplot, but code is available at StackOver flow
# http://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
ggMMplot <- function(var1, var2){
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) 
  }


##***part016b;
# There is no builtin mosaic plot in ggplot, but code is available at StackOver flow
# http://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2

mj.expand <- mj[rep(1:nrow(mj), mj$Count),]
mplot <- ggMMplot(mj.expand$Parental.usage, mj.expand$Student.usage)+
    ggtitle("Mosaic plot of distribution of Student usage by Parental usage ")+
    xlab("Parental Usage")+ylab("Proportion")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='marijuana-short-R-016.png', h=4, w=6, unit="in", dpi=300)


mosaicplot(table.mj)  


# Do the formal chisquare goodness of fit test
sink('marijuana-short-R-020.txt', split=TRUE)
##***part020b;
mj.test <- chisq.test(table.mj)
mj.test
##***part020e;
sink()

sink("marijuana-short-R-epiodds.txt", split=TRUE)
##***partepioddsb;
# Compute odds ratio's using the epitools package
or <-epitools::oddsratio(table.mj)
or
##***partepioddse;
sink()


sink('marijuana-short-R-glmfit.txt', split=TRUE)
##***partglmfitb;
# Do the equivalent logistic regression
# Convert to a proportion of Student.usage=yes for each parental usage
mj2 <- ddply(mj, "Parental.usage", function(x){
     totaln <- sum(x$Count)
     pyes   <- x$Count[ x$Student.usage =='yes']/totaln
     res    <- data.frame(totaln=totaln, pyes=pyes)
     return(res)
})
mj2
mj.fit <- glm(Student.usage ~ Parental.usage, data=mj, weight=Count, family=binomial(link=logit))
anova(mj.fit, test='Chisq')
##***partglmfite;
sink()

sink('marijuana-short-R-lsmo.txt', split=TRUE)
##***partlsmob;
cat('Estimated log-odds of student usage for each level of parental usage\n')
mj.fit.lsmo <- lsmeans::lsmeans(mj.fit, ~Parental.usage)
summary(mj.fit.lsmo)
mj.logoddratio <- pairs(mj.fit.lsmo, reverse=TRUE)
cat('Estimated odds ratio and confidence bounds\n')
exp(confint(mj.logoddratio)[,c("estimate","asymp.LCL","asymp.UCL")])
##***partlsmoe;
sink()