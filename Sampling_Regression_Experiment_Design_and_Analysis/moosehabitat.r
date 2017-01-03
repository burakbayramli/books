# Moose Habitat Selection
# Reading of habitat selection by moose are compared 
# to known areas from the map.
#  Data from Manly's book on Resource Selection Functions.

# 2014-12-19 CJS ggplot; 

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load libraries needed
library(ggplot2)
library(plyr)


# Read in the data
sink('moosehabitat-R-001.txt', split=TRUE)
##***part001b;
tracking <- read.csv("moosehabitat.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
tracking$Habitat <- factor(tracking$Habitat)
tracking
##***part001e;
sink()

str(tracking)

   
     
     
# Compute standard errors for each proportion
# There is no easy way to do this in R other than BFI computations
# Note that the formula for the SE is ONLY valid in simple random sampling

sink('moosehabitat-R-005.txt', split=TRUE)
##***part005b;
total.tracking <- sum(tracking$MooseLocations)
tracking$EstProp <- tracking$MooseLocations/total.tracking
tracking$se <- sqrt(tracking$EstProp *(1-tracking$EstProp)/total.tracking)
tracking$lcl<- tracking$EstProp - 2*tracking$se
tracking$ucl<- tracking$EstProp + 2*tracking$se
tracking
##***part005e;
sink()


# Make a segmented bar chart comparing the observed and actual proportions

##***part004b;
# Convert from wide to long format for plotting purposes

tracking.moose <- data.frame(source="Moose", Habitat=tracking$Habitat, prop=tracking$EstProp, tracking[,c("se","lcl","ucl")])
tracking.map   <- data.frame(source="Actual",Habitat=tracking$Habitat, prop=tracking$RealProp)
tracking.long  <- rbind.fill(tracking.moose, tracking.map) 
tracking.long  <- tracking.long[ order(tracking.long$source, tracking.long$Habitat),]

segbar <- ggplot(data=tracking.long, aes(x=source, y=prop, fill=Habitat ))+
  ggtitle("Stacked bar chart comparing proportions in each habitat")+
  xlab("Type of data")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='moosehabitat-R-004.png', h=4, w=6, units="in", dpi=300)
  
# Using base R graphics
barplot(cbind(tracking$MooseLocations/sum(tracking$MooseLocations),tracking$RealProp),
        legend=tracking$Habitat, 
        names.arg=c("Actual","Theoretical"),
        main='Segmented bar charts')


# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=tracking.long, aes(x=Habitat, y=prop, fill=source))+
  ggtitle("Comparing observed and theroetical proportions")+
  ylab("Proportion in each habitat (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="moosehabitat-R-003.png", h=4, w=6, units="in", dpi=300)

# Using Base-R graphics 
# You could add confidence intervals for each segment using the segments() function.
barplot(cbind(tracking$MooseLocations/sum(tracking$MooseLocations),tracking$RealProp),
        legend=tracking$Habitat, 
        names.arg=c("Actual","Theoretical"),
        beside=TRUE)



# Do the formal chisquare goodness of fit test
sink('moosehabitat-R-006.txt', split=TRUE)
##***part006b;
hab.test <- chisq.test(tracking$MooseLocations, p=tracking$RealProp)
hab.test

# There is no built-in G2 test in R, but is easily computed
# once you have the observed and expected counts from the chi-square test above
# The df is the same as the chi-square test
g2.test <- with(hab.test,2*sum(observed * log(observed/expected), na.rm=TRUE))
g2.pvalue <- 1-pchisq(g2.test,hab.test$parameter)
cat("G2 test for independence: ", round(g2.test,2),' with ', hab.test$parameter," df ",
    ' with p-value ',round(g2.pvalue,3),'\n\n')
##***part006e;
sink()

