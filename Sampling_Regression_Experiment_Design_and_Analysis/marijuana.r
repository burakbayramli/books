# Student and Parental Usage of Marijuana

# Are the sins of the parents visited upon their children.
# Students in a college were interviewed about their and their parents
# use of marijuana and alcohol.

# 2015-04-17 CJS misc changes
# 2014-12-20 CJS ggplot; as.is; split-TRUE, etc

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)


# Read in the data
sink('marijuana-R-001.txt', split=TRUE)
##***part001b;
high <- read.csv("marijuana.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
high$parental   <- factor(high$parental, level=c("Neither","One","Both"), order=TRUE)
high$student    <- factor(high$student,  level=c("Never","Occasional","Regular"), order=TRUE)
high
##***part001e;
sink()

str(high)


# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('marijuana-R-015.txt', split=TRUE)
##***part015b;
table.high <- xtabs(count ~parental + student, data=high)
table.high 
prop.table(table.high,margin=1)
##***part015e;
sink()


# Compute the proportions and se/ci for each proportion for each type of data
sink('marijuana-R-002.txt', split=TRUE)
##***part002b;
report <- ddply(high, "parental", function(x){
  # Compute the proportions etc for each student of count
  x$prop <- x$count/ sum(x$count)
  x$se   <- sqrt(x$prop*(1-x$prop)/x$count)
  x$lcl  <- x$prop - 2*x$se
  x$ucl  <- x$prop + 2*x$se
  return(x)
})
report
##***part002e;
sink()



##***part004b;
segbar <- ggplot(data=report, aes(x=parental, y=prop, fill=student ))+
  ggtitle("Stacked bar chart comparing proportions of usage by parental usage")+
  xlab("Parental usage")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='marijuana-R-segbar.png', h=4, w=6, units="in", dpi=300)


# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=student, y=prop, fill=parental))+
  ggtitle("Comparing proportions of usage from two students")+
  ylab("Proportion in each usage class (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="marijuana-R-barplot.png", h=4, w=6, units="in", dpi=300)


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

high.expand <- high[rep(1:nrow(high), high$count),]
mplot <- ggMMplot(high.expand$parental, high.expand$student)+
    ggtitle("Mosaic plot of distribution of student usage by parental usage")+
    xlab("Parental Usage")+ylab("Proportion")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='marijuana-R-016.png', h=4, w=6, unit="in", dpi=300)


mosaicplot(table.high)  


# Do the formal chisquare goodness of fit test
sink('marijuana-R-020.txt', split=TRUE)
##***part020b;
high.test <- chisq.test(table.high)
high.test
high.test$observed
high.test$expected
high.test$residual
##***part020e;
sink()

png('marijuana-R-030.png')
##***part030b;
assocplot(table.high)   
##***part030e;
dev.off() 

assocplot(table.high)   

 