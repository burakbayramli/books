# Car Poll

# A sample of people were interviewed about their preferences for car.
# This is the Car Poll.jmp data file that ships with JMP.

# 2014-12-20 CJS ggplot; as.is; split=TRUE

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)
library(reshape2)


# Read in the data
sink('carpoll-R-001.txt')
##***part001b;
poll <- read.csv("carpoll.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
poll$sex            <- factor(poll$sex)
poll$marital.status <- factor(poll$marital.status)
poll$country        <- factor(poll$country)
poll$size           <- factor(poll$size)
poll$type           <- factor(poll$type)
poll[1:5,]
##***part001e;
sink()

str(poll)


# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('carpoll-R-015.txt')
##***part015b;
table.poll <- xtabs( ~sex + country, data=poll)
table.poll 
prop.table(table.poll,margin=1)
##***part015e;
sink()

# Compute the proportions and se/ci for each proportion for each type of data
sink('elkhabitat-R-002.txt', split=TRUE)
##***part002b;
report <- ddply(poll, "sex", function(x){
  # Compute the proportions etc for each source of points
  # First get county by country of manufacturer
  xx <- ddply(x, "country", function(xxx){count<-data.frame(count=nrow(xxx)); return(count)})
  xx$prop <- xx$count/ sum(xx$count)
  xx$se   <- sqrt(xx$prop*(1-xx$prop)/xx$count)
  xx$lcl  <- xx$prop - 2*xx$se
  xx$ucl  <- xx$prop + 2*xx$se
  return(xx)
})
report
##***part002e;
sink()


##***part004b;
segbar <- ggplot(data=report, aes(x=sex, y=prop, fill=country ))+
  ggtitle("Stacked bar chart comparing proportions of preferance by sex")+
  xlab("Sex")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='carpoll-R-segbar.png', h=4, w=6, units="in", dpi=300)



# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=country, y=prop, fill=sex))+
  ggtitle("Comparing proportions of country of manufacturing between sexes")+
  ylab("Proportion  (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="carpoll-R-barplot.png", h=4, w=6, units="in", dpi=300)


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

mplot <- ggMMplot(poll$sex, poll$country)+
    ggtitle("Mosaic plot of distribution of canopy class in elk usage and random points")+
    xlab("Sex")+ylab("Proportion")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='carpoll-R-016.png', h=4, w=6, unit="in", dpi=300)



# R's mosaic plot
mosaicplot(table.poll)  



# Do the formal chisquare goodness of fit test
sink('carpoll-R-020.txt')
##***part020b;
poll.test <- chisq.test(table.poll)
poll.test
poll.test$observed
poll.test$expected
poll.test$residual
##***part020e;
sink()

png('carpoll-R-030.png')
##***part030b;
assocplot(table.poll)   
##***part030e;
dev.off() 

assocplot(table.poll)
     















