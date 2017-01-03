# Homicide Numbers by Season of Occurance

# Is there evidence that homicides change by season?

# 2014-12-19 CJS ggplot; split=TRUE; as.is=TRUE

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# load libraries needed
library(ggplot2)
library(plyr)


# Read in the data
sink('homicideseason-R-001.txt', split=TRUE)
##***part001b;
homicide <- read.csv("homicideseason.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
homicide$Season <- factor(homicide$Season, levels=c("Spring","Summer","Fall","Winter"), ordered=TRUE)
homicide$HypProp <- .25
homicide
##***part001e;
sink()

str(homicide)


     
# Compute standard errors for each proportion
# There is no easy way to do this in R other than BFI computations
# Note that the formula for the SE is ONLY valid in simple random sampling

sink('homicideseason-R-005.txt', split=TRUE)
##***part005b;
total.homicide <- sum(homicide$Homicides)
homicide$EstProp <- homicide$Homicides/total.homicide
homicide$se <- sqrt(homicide$EstProp*(1-homicide$EstProp)/total.homicide)
homicide$lcl<- homicide$EstProp - 2*homicide$se
homicide$ucl<- homicide$EstProp + 2*homicide$se
homicide
##***part005e;
sink()


# Make a segmented bar chart comparing the observed and actual proportions

##***part004b;
# Convert from wide to long format for plotting purposes

homicide.murder <- data.frame(source="Murders", Season=homicide$Season, prop=homicide$EstProp, homicide[,c("se","lcl","ucl")])
homicide.season <- data.frame(source="Theoretical",Season=homicide$Season, prop=homicide$HypProp)
homicide.long  <- rbind.fill(homicide.murder, homicide.season) 
homicide.long  <- homicide.long[ order(homicide.long$source, homicide.long$Season),]

segbar <- ggplot(data=homicide.long, aes(x=source, y=prop, fill=Season ))+
  ggtitle("Stacked bar chart comparing proportions in each Season")+
  xlab("Type of data")+ylab("Proportion")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;

ggsave(plot=segbar, file='homicideseason-R-004.png', h=4, w=6, units="in", dpi=300)
  
barplot(cbind(homicide$Homicides/sum(homicide$Homicides),homicide$HypProp),
        legend=homicide$Season, 
        names.arg=c("Actual","Theoretical"),
        main='Segmented bar charts')




# Create side-by-side bar charts
##***part003b;
barplot <- ggplot(data=homicide.long, aes(x=Season, y=prop, fill=source))+
  ggtitle("Comparing observed and theroetical proportions")+
  ylab("Proportion in each Season (with 95% ci)")+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), w=0.2, position=position_dodge(w=0.9))+
  scale_fill_grey(start=0.4, end=0.8)
barplot
##***part003e;

ggsave(plot=barplot, file="homicideseason-R-003.png", h=4, w=6, units="in", dpi=300)

# Create a segmented bar chart
# You could add confidence intervals for each segment using the segments() function.
barplot(cbind(homicide$Homicides/sum(homicide$Homicides),homicide$HypProp),
        legend=homicide$Season, 
        names.arg=c("Actual","Theoretical"),
        beside=TRUE)





# Do the formal chisquare goodness of fit test
sink('homicideseason-R-006.txt', split=TRUE)
##***part006b;
homicide.test <- chisq.test(homicide$Homicides, p=homicide$HypProp)
homicide.test

# There is no built-in G2 test in R, but is easily computed
# once you have the observed and expected counts from the chi-square test above
# The df is the same as the chi-square test
g2.test <- with(homicide.test,2*sum(observed * log(observed/expected), na.rm=TRUE))
g2.pvalue <- 1-pchisq(g2.test,homicide.test$parameter)
cat("G2 test for independence: ", round(g2.test,2),' with ', homicide.test$parameter," df ",
    ' with p-value ',round(g2.pvalue,3),'\n\n')

##***part006e;
sink()











