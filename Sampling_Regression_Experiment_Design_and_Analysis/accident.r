# Accidents classified by outcome

# 2014-12-20 CJS gplot; split; as.is;

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)
library(reshape2)

# Read in the data
sink('accident-R-001.txt', split=TRUE)
##***part001b;
accident <- read.csv("accident.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
accident$Cause   <- factor(accident$Cause)
accident$Outcome <- factor(accident$Outcome)
accident
##***part001e;
sink()

str(accident)


# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('accident-R-015.txt', split=TRUE)
##***part015b;
table.accident <- xtabs(Count ~Cause + Outcome, data=accident)
table.accident 
prop.table(table.accident,margin=1)
##***part015e;
sink()


# Compute our own proportions and se for plot
report <- ddply(accident, "Cause", function(x){
  # Compute the proportions etc for each source of Count
  x$prop <- x$Count/ sum(x$Count)
  x$se   <- sqrt(x$prop*(1-x$prop)/x$Count)
  x$lcl  <- x$prop - 2*x$se
  x$ucl  <- x$prop + 2*x$se
  return(x)
})
report




##***part004b;
segbar <- ggplot(data=report, aes(x=Cause, y=prop, fill=Outcome ))+
  ggtitle("Stacked bar chart comparing p(fatal) by cause of accident")+
  xlab("Cause of accident")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='accident-R-segbar.png', h=4, w=6, units="in", dpi=300)


# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=Cause, y=prop, fill=Outcome))+
  ggtitle("Comparing proportions of outcome by cause")+
  ylab("Proportion in each outcome (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="elkhabitat-R-barplot.png", h=4, w=6, units="in", dpi=300)


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

accident.expand <- accident[rep(1:nrow(accident), accident$Count),]
mplot <- ggMMplot(accident.expand$Cause, accident.expand$Outcome)+
    ggtitle("Mosaic plot of distribution of outcome by cause of accident")+
    xlab("Cause of accident")+ylab("Proportion")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='elkhabitat-R-016.png', h=4, w=6, unit="in", dpi=300)

# also in base $ graphics
mosaicplot(table.accident)  


# Do the formal chisquare goodness of fit test
sink('accident-R-020.txt', split=TRUE)
##***part020b;
accident.test <- chisq.test(table.accident)
accident.test
accident.test$observed
accident.test$expected
accident.test$residual
##***part020e;
sink()

# R has a funky association plot, but side-by-side bar plots show the same information
png('accident-R-030.png')
##***part030b;
assocplot(table.accident)   
##***part030e;
dev.off()

assocplot(table.accident)   


 