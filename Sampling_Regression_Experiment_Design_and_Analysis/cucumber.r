# Estimating numbers of sea cucumbers using a survey	
# 2015-07-04 CJS update split; ##***; ggplot2

# Sea cucumbers are considered a delicacy among some, and the fishery
#   is of growing importance.

#   In order to set harvest quotas and in order to monitor the stock,
#   it is important that number of sea cucumbers in a certain harvest area
#   be estimated each year.
#
#   The following is an example taken from Griffith Passage in  BC 1994.

#   To do this, the managers lay out a number of transects
#   across the cucumber harvest area.
#   Divers then swim along the transect, and while carrying a 4 m wide
#   pole, count the number of cucumbers within the width of the pole
#   during the swim.

#   The number of possible transects is so large that the correction
#   for finite population sampling can be ignored. 

#   The total bed area and the length of the shore are:

#   Bed Area        3,769,280       m^2
#   Shorelength      51426       m

#   The raw data is already summarized to the transect level so a simple ratio estimator is all that is needed  

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)
library(survey)

# Read in the data
sink('cucumber-R-001.txt', split=TRUE)
##***part001b;
cucumber <- read.csv("cucumber.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
Ntransects <- 51426/4 # each transect is 4 m wide
TotalArea <- 3769280
cucumber
##***part001e;
sink()



# Analysis as a simple random sample, ignoring the area and getting a simple inflation estimator
sink('cucumber-R-002.txt', split=TRUE)
##***part002b;
mean.cucumber      <- mean(cucumber$Cucumber)
sd.cucumber        <- sd(cucumber$Cucumber)
se.mean.cucumber   <- sd.cucumber / sqrt(length(cucumber$Cucumber))
cat(' Est Mean cucumber/trnsect is ',mean.cucumber,' with a se of',
      se.mean.cucumber,"\n")
      
total.cucumber    <- mean.cucumber    * Ntransects
se.total.cucumber <- se.mean.cucumber * Ntransects
cat(' Est Total cucumber is ',total.cucumber,' with a se of',
      se.total.cucumber,"\n")
##***part002e;
sink()


# Analysis as a simple random sample, using the survey package
sink('cucumber-R-003.txt', split=TRUE)
##***part003b;
library(survey)
# Add a variable for the number of subarea in the population
cucumber$Ntransects <- Ntransects
cucumber[1:5,]
cucumber.design <- svydesign(data=cucumber, 
      ids=~1, # no clusters
      fpc=~Ntransects)  # boats in the day
print(cucumber.design)
mean.cucumber2  <- svymean(  ~Cucumbers, design=cucumber.design)
mean.cucumber2
total.cucumber2 <- svytotal( ~Cucumbers, design=cucumber.design)
total.cucumber2
##***part003e;
sink()



##***part011b;
prelim <- ggplot(data=cucumber, aes(x=Area, y=Cucumbers))+
  ggtitle("cucumber vs. area")+
  geom_point(size=4)
prelim
##***part011e;
ggsave(prelim, file='cucumber-R-011.png', h=4, w=6, units="in", dpi=300)



#************ Analysis using the survey package and a ratio estimator **********

# For a proper analysis of survey data, you should construct
# a survey.design object so that subsequent analysis can
# account for the design, the fpc, etc.
# This is likely overkill for simple random samples and simple
# means and totals

sink('cucumber-R-020.txt', split=TRUE)
##***part020b;
library(survey)
cucumber$Ntransects <- Ntransects
cucumber[1:5,]
cucumber.design <- svydesign(data=cucumber, 
      ids=~1, # no clusters
      fpc=~Ntransects)  # boats in the day
print(cucumber.design)
cucumber.ratio <- svyratio(numerator=~Cucumbers,
               denominator=~Area, cucumber.design)
cucumber.ratio
cucumber.total <- predict(cucumber.ratio, total=TotalArea)
cucumber.total
##***part020e;
sink()
