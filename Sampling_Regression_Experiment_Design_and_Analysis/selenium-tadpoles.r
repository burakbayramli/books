# Impact of different levels of Selenium on the deformity proportions of tadpoles
#
# Frog eggs were placed into containers with different levels of Se (one per concentration)
# The number of defomities was counted when the eggs hatched.
# 
# Note that there is an issue of pseudo-replication here (why?)
#
# Change log
#   2015-07-07 CJS Updated with ggplot; lsmeans etc.
#   2013-10-07 CJS First edition
#
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#
options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(lsmeans)
library(plyr)
library(reshape2)
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")

# Define the logit and expit transform
logit <- function(p){log(p/(1-p))}
expit <- function(logit){1/(1+exp(-logit))}

logit(seq(.1,.9,.1))
expit(logit(seq(.1,.9,.1)))



# Read in the data
sink('selenium-tadpoles-R-data.txt', split=TRUE)
##***part001b;
# Read in the raw data
tadpoles <- read.csv("selenium-tadpoles.csv", header=TRUE, as.is=TRUE)

# Create the selenium level as an ORDERED factor
tadpoles$Selenium <- factor(tadpoles$Selenium, 
                            levels=c("Control","low","medium","high"), order=TRUE)
head(tadpoles)
##***part001e;
sink()


sink('selenium-tadpoles-R-summary.txt', split=TRUE)
##***partsummaryb;
# Summary table
table.tadpoles <- xtabs(Count ~Selenium + Status, data=tadpoles)
table.tadpoles 
prop.tadpoles <- prop.table(table.tadpoles,margin=1)
# add the odds ratio
prop.tadpoles <- cbind(prop.tadpoles, logit.def=logit(prop.tadpoles[,1]/100))
round(prop.tadpoles,3)
##***partsummarye;
sink()


##***part004b;
tadpoles$pdeform <- tadpoles$Count/tadpoles$TotalTadpoles
segbar <- ggplot(data=tadpoles, aes(x=Selenium, y=pdeform, fill=Status ))+
  ggtitle("Comparing p(deformity) by selenium level")+
  xlab("Selenium leverl")+ylab("Proportion ")+
  geom_bar(stat="identity", color="black")+
  scale_fill_grey()
segbar
##***part004e;
ggsave(plot=segbar, file='selenium-tadpoles-R-barchart.png', h=4, w=6, units="in", dpi=300)


sink("selenium-tadpoles-R-chisq.txt", split=TRUE)
##***partchisqb;
# Formal chi-square tests
tadpoles.test <- chisq.test(table.tadpoles)
tadpoles.test
# Note that R does NOT have the G2 (likelhood ratio) test, but it is easily computed
# and it has the same df as the Pearson chi-square test
# See http://www.pmc.ucsc.edu/~mclapham/Rtips/categorical.htm for details on a R function to do this
cat("G2 test results\n")
G2 <- 2 * sum(tadpoles.test$observed * log(tadpoles.test$observed/tadpoles.test$expected))
cat("The value of G2 is ",G2, " with pvalue ", 
      pchisq(G2, tadpoles.test$parameter, lower.tail=FALSE), "\n")
##***partchisqe;  
sink()
                       
                       
# Problem is that the chi-square test doesn't allow for multiple comparisons
# We need to use a glm (logisitic regression)

sink('selenium-tadpoles-R-glmfit.txt', split=TRUE)
##***partglmfitb;
# Reshape the data to make it suitable for glm
# Because the data has been summarized, we want one line per Se level with two variables 
# representing the number of deformaties and non-deformaties
tadpoles.wide <- reshape2::dcast(tadpoles, Selenium ~ Status,   value.var="Count" )
names(tadpoles.wide)[c(2,3)]<-c("Deformed","Not.Deformed") # remove blank from variable name
tadpoles.wide

# Now fit the logistic model and test for effects
tadpoles.glm <- glm( cbind(Deformed,Not.Deformed) ~ Selenium, data=tadpoles.wide,
                     family=binomial(link=logit) )
anova(tadpoles.glm, test="Chisq")
##***partglmfite;
sink()


sink("selenium-tadpoles-R-lsmo.txt", split=TRUE)
##***partlsmob;
# Get the marginal means and cld report
tadpoles.glm.lsmo <- lsmeans::lsmeans(tadpoles.glm, ~Selenium)
cat("Marginal responses on the logit scale\n")
summary(tadpoles.glm.lsmo)
cat("Marginal responses on the probability scale\n")
summary(tadpoles.glm.lsmo, type=c("response"))
cat("Differences on the logit scale - log(odds ratios)\n")
tadpoles.glm.lsmo.pairs <- confint(pairs(tadpoles.glm.lsmo))
tadpoles.glm.lsmo.pairs
cat("Odds ratio\n")
cbind( contrast=tadpoles.glm.lsmo.pairs[,"contrast"], exp( tadpoles.glm.lsmo.pairs[,c("estimate","asymp.LCL","asymp.UCL")]))
cat("Multiple comparison of logits\n")
tadpoles.glm.cld <- cld(tadpoles.glm.lsmo)
tadpoles.glm.cld
##***partlsmoe;
sink()

sf.cld.plot.bar(tadpoles.glm.cld, "Selenium")

# Make a plot of the lsmeans
cldplot <- sf.cld.plot.line(tadpoles.glm.cld, "Selenium")+
     ylab("logit(p(deformity)) with 95% ci")+xlab("Selenium levels")+
     ggtitle("logit(deformity) by selenium level")
cldplot
ggsave(plot=cldplot, file='selenium-tadpoles-R-cldplot.png', h=4, w=6, units="in", dpi=300)


# Model assessment plots are silly because model is "perfect" fit to the data.



