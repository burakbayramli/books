# Feeding times of sandpipers

# A student went to Boundary Bay (bb) or Sidney Island (si) and measured
# the time flocks of sandpipers spent feeding.
# If the time was more than 10 minutes, this flock was abandoned and the
# time censored at 600 seconds.
# The time-block variable split the feeding time into 3 categories - < 5 
# minutes, 5-10 minutes, and 10+ minutes.

# 2015-04-20 CJS misc changes
# 2014-12-20 CJS ggplot; as.is; split=TRUE
# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)

# Read in the data
sink('flocks-R-001.txt')
##***part001b;
flocks <- read.csv("flocks.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
flocks$site       <- factor(flocks$site)
flocks$time.interval <- factor(flocks$time.interval, levels=c("00-4.9999","05-9.9999","10+"), order=TRUE)
flocks[1:5,]
##***part001e;
sink()

str(flocks)

# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('flocks-R-015.txt')
##***part015b;
table.flocks <- xtabs( ~site + time.interval, data=flocks)
table.flocks 
prop.table(table.flocks,margin=1)
##***part015e;
sink()


# Compute the proportions and se/ci for each proportion for each type of data
sink('flocks-R-002.txt', split=TRUE)
##***part002b;
report <- ddply(flocks, "site", function(x){
  # Compute the proportions etc for each site of points
  xx <- ddply(x, "time.interval", function(xxx){count<-data.frame(count=nrow(xxx)); return(count)})
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
segbar <- ggplot(data=report, aes(x=site, y=prop, fill=time.interval))+
  ggtitle("Stacked bar chart comparing proportions in each time interval")+
  xlab("Site")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='flocks-R-segbar.png', h=4, w=6, units="in", dpi=300)



# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=time.interval, y=prop, fill=site))+
  ggtitle("Comparing proportions of usage from two sites")+
  ylab("Proportion  (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="flocks-R-barplot.png", h=4, w=6, units="in", dpi=300)

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

mplot <- ggMMplot(flocks$site, flocks$time.interval)+
    ggtitle("Mosaic plot of distribution of usage by site")+
    xlab("Site")+ylab("Proportional usage")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='flocks-R-016.png', h=4, w=6, unit="in", dpi=300)

# Base R graphics
mosaicplot(table.flocks)  


# Do the formal chisquare goodness of fit test
sink('flocks-R-020.txt')
##***part020b;
flocks.test <- chisq.test(table.flocks)
flocks.test
flocks.test$observed
flocks.test$expected
flocks.test$residual
##***part020e;
sink()

png('flocks-R-030.png')
##***part030b;
assocplot(table.flocks)   
##***part030e;
dev.off()

assocplot(table.flocks)
 