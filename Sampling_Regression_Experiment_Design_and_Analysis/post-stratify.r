# Post-stratification example
# 2015-02-21 CJS ##***; sink; as.is;

# A survey of 20 x 1 m2 plots was taken from a study area of 100 m2. 
# In each quadrat the number of grubs was measured. At the same time, 
# a post-stratification into high and low quality habitats was done. 
# It was subsequently determined that there are 30 m2 of high quality 
# and 70 m2 of low quality habitat in the study area.

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects
library(plyr)
library(survey)

# Read in the data
sink('post-stratify-R-001.txt', split=TRUE)
##***part001b;
grubs <- read.csv("post-stratify.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
grubs
##***part001e;
sink()


#************ Analysis using standard R functions ***********
# These typically do not include the effects of the finite population
# correction factor, but will be good enough.
#
  
 
# Use the t.test function to do the same but we need to compute the se
# based on the statistic and the estimate
sink('post-stratify-R-002.txt', split=TRUE)
##***part002b;
t.test.Grubs <- t.test(grubs$Grubs)
names(t.test.Grubs)
t.test.Grubs$se.mean <- t.test.Grubs$estimate / t.test.Grubs$statistic
cat("Est Mean Grubs per boat is ", t.test.Grubs$estimate,
    ";\n    se of mean(Grubs per boat) is ", t.test.Grubs$se.mean,"\n\n")
##***part002e;
sink()


# Get the estimates for each stratum 
sink('post-stratify-R-003.txt', split=TRUE)
##***part003b;
summary.Grubs <- ddply(grubs, "Post.strata", summarize,
                n   =length(Grubs),
                mean=mean  (Grubs),
                sd  =sd    (Grubs),
                se.mean=sd (Grubs)/ sqrt(length(Grubs)))
summary.Grubs
##***part003e;
sink()

# Roll up
sink('post-stratify-R-004.txt', split=TRUE)
##***part004b;
ExpFactor <- data.frame(Post.strata=c('h','l'), area=c(30,70))
summary.Grubs <- merge(summary.Grubs, ExpFactor)
summary.Grubs$Grubs.total   <- summary.Grubs$mean     * summary.Grubs$area
summary.Grubs$se.Grubs.total<- summary.Grubs$se.mean  * summary.Grubs$area
summary.Grubs
cat('Estimate total grubs ',sum(summary.Grubs$Grubs.total),
    ';\n with a se of ',sqrt(sum(summary.Grubs$se.Grubs.total**2)),"\n")

##***part004e;
sink()
