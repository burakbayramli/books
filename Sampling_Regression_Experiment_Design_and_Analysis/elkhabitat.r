# Elk CanopyClass

# Elk were radio collared and located in various CanopyClasss. 
# Random points were also located on a map and the CanopyClass noted.
# This is taken from Manly's book on Resource Selection Functions.

# 2014-12-19 CJS ggplot; as.is=TRUE; split=TRUE

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load required libraries
library(ggplot2)
library(plyr)
library(reshape2)

# Read in the data
sink('elkhabitat-R-001.txt', split=TRUE)
##***part001b;
tracking <- read.csv("elkhabitat.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
tracking$CanopyClass <- factor(tracking$CanopyClass)
tracking
##***part001e;
sink()

str(tracking)

# Compute the proportions and se/ci for each proportion for each type of data
sink('elkhabitat-R-002.txt', split=TRUE)
##***part002b;
tracking2 <- melt(tracking, id="CanopyClass", variable.name="source", value.name="points")
tracking2

report <- ddply(tracking2, "source", function(x){
  # Compute the proportions etc for each source of points
  x$prop <- x$points/ sum(x$points)
  x$se   <- sqrt(x$prop*(1-x$prop)/x$points)
  x$lcl  <- x$prop - 2*x$se
  x$ucl  <- x$prop + 2*x$se
  return(x)
})
report
##***part002e;
sink()



##***part004b;
segbar <- ggplot(data=report, aes(x=source, y=prop, fill=CanopyClass ))+
  ggtitle("Stacked bar chart comparing proportions in each Canopy Class")+
  xlab("Type of data")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='elkhabitat-R-004.png', h=4, w=6, units="in", dpi=300)


barplot(cbind(tracking$ElkUsage/sum(tracking$ElkUsage),
              tracking$RandomPoints/sum(tracking$RandomPoints)),
        legend=tracking$CanopyClass, 
        names.arg=c("Actual","Theoretical"),
        main='Segmented bar charts')




# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=report, aes(x=CanopyClass, y=prop, fill=source))+
  ggtitle("Comparing proportions of usage from two sources")+
  ylab("Proportion in each canopy class (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="elkhabitat-R-003.png", h=4, w=6, units="in", dpi=300)

# Create a segmented bar chart
# You could add confidence intervals for each segment using the segments() function.
barplot(cbind(tracking$ElkUsage/sum(tracking$ElkUsage),
              tracking$RandomPoints/sum(tracking$RandomPoints)),
        legend=tracking$CanopyClass, 
        names.arg=c("Actual","Theoretical"),
        beside=TRUE)





# The data needs to be in  "long" format
# You need a variable for the two classifications and a frequence variable.
# This was created in the report variable previously

# The melt function in from the reshape2 package. 
sink('elkhabitat-R-010.txt', split=TRUE)  
##***part010b;
tracking.long <- melt(tracking, id="CanopyClass", variable.name="source", value.name="points")
head(tracking.long)
##***part010e;
sink()
  

# We now create a contingency table which can be used in various analyses
# Try different order in the variable on the right of the ~ and see the effect on the
# table and plot.

sink('elkhabitat-R-015.txt', split=TRUE)
##***part015b;
table.tracking <- xtabs(points ~ source + CanopyClass , data=tracking.long)
table.tracking 
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

tracking.expand <- tracking.long[rep(1:nrow(tracking.long), tracking.long$points),]
mplot <- ggMMplot(tracking.expand$source, tracking.expand$CanopyClass)+
    ggtitle("Mosaic plot of distribution of canopy class in elk usage and random points")+
    xlab("Source of points")+ylab("Proportional usage")+
    scale_fill_grey(start=0.4, end=0.8)
mplot
##***part016e;
ggsave(plot=mplot, file='elkhabitat-R-016.png', h=4, w=6, unit="in", dpi=300)


# Alternatively
# There is no mosaic plot in ggplot, but Wikham as a package on GitHub which is useful.
# See http://vita.had.co.nz/papers/prodplots.pdf for details
library(devtools)
install_github("hadley/productplots")
library(productplots)

# Notice we need to expand the number of rows by the count (the points variable)
# Refer to http://stackoverflow.com/questions/2894775/how-can-you-replicate-each-row-of-an-r-data-frame-and-specify-the-number-of-repl
mplot <- prodplot(data=tracking.expand, ~ CanopyClass + source)+
    ggtitle("Mosaic plot of distribution of canopy class in elk usage and random points")+
    xlab("Source of points")
mplot

# Base R graphics also has a crude plot
mosaicplot(table.tracking)  

# The vcd package also has nice stuff
library(vcd); library(grid)
vcd::mosaic(CanopyClass ~ source,  data=tracking.expand)  


sink('elkhabitat-R-017.txt', split=TRUE)
##***part017b;
prop.table(table.tracking,margin=1)
##***part017e;
sink()


# Do the formal chisquare goodness of fit test
sink('elkhabitat-R-020.txt', split=TRUE)
##***part020b;
hab.test <- chisq.test(table.tracking)
hab.test
hab.test$observed
hab.test$expected
hab.test$residual
##***part020e;
sink()

# R has a funky association plot
png('elkhabitat-R-030.png')
##***part030b;
assocplot(table.tracking)
vcd::assoc(CanopyClass ~ source,  data=tracking.expand)
##***part030e;
dev.off()

assocplot(table.tracking)
vcd::assoc(CanopyClass ~ source,  data=tracking.expand)

     


