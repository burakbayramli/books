# Oyster Leases

# Does ownership type influence the outlook for oyster leases?

# 2014-12-20 CJS ggplot; as.is; split=TRUE

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)


# Read in the data
sink('oyster-lease-R-001.txt', split=TRUE)
##***part001b;
lease <- read.csv("oyster-lease.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
lease$ownership   <- factor(lease$ownership)
lease$outlook     <- factor(lease$outlook)
lease
##***part001e;
sink()

str(lease)


# Compute the proportions and se/ci for each proportion for each type of data
sink('oyster-lease-R-002.txt', split=TRUE)
##***part002b;
report <- ddply(lease, "ownership", function(x){
  # Compute the proportions etc for each ownership of count
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
segbar <- ggplot(data=report, aes(x=ownership, y=prop, fill=outlook ))+
  ggtitle("Stacked bar chart comparing proportions in each ownership class")+
  xlab("Ownership class")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='oyster-lease-R-004.png', h=4, w=6, units="in", dpi=300)


# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=outlook, y=prop, fill=ownership))+
  ggtitle("Comparing proportions of outlook by ownerships")+
  ylab("Proportion in each class (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="oyster-lease-R-barplot.png", h=4, w=6, units="in", dpi=300)




# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('oyster-lease-R-015.txt', split=TRUE)
##***part015b;
table.lease <- xtabs(count ~ ownership + outlook , data=lease)
table.lease 
prop.table(table.lease,margin=1)
##***part015e;
sink()


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

lease.expand <- lease[rep(1:nrow(lease), lease$count),]
mplot <- ggMMplot(lease.expand$ownership, lease.expand$outlook)+
    ggtitle("Mosaic plot of distribution of outlook by ownership")+
    xlab("Ownership")+ylab("Proportion")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='oyster-lease-R-016.png', h=4, w=6, unit="in", dpi=300)


# Base R graphics
mosaicplot(table.lease)  



# Do the formal chisquare goodness of fit test
sink('oyster-lease-R-020.txt', split=TRUE)
##***part020b;
oyster.test <- chisq.test(table.lease)
oyster.test
oyster.test$observed
oyster.test$expected
oyster.test$residual
##***part020e;
sink()

# R's funky plot
png('oyster-lease-R-030.png')
##***part030b;
assocplot(table.lease)   
##***part030e;
dev.off() 

assocplot(table.lease)   
     


