# Do growth hormones affect the final weight of cattle?

# Cattle were randomly assigned to receive either injections of
# growth hormone or of placebo. The final weight of the cattle 
# was recorded. Two cows were struck by lightening and died.

# 2015-04-18 CJS remove doby; fixups; 
# 2014-01-27 CJS Fixed error in computing the se
#                Added code for strip chart in ggplot
#                Use plyr

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case of no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)

sink('hormone-R-000.txt', split=TRUE)
# Read in the data
##***part001b;
cows.raw <- read.csv('hormone.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
cows.raw
##***part001e;
sink()


sink('hormone-R-000b.txt', split=TRUE)
# Stack the two variables and rename
##***part001bb;
cows <- stack(cows.raw)
names(cows)<-c("Weight","Treatment")
cows$Treatment <- factor(cows$Treatment)
cows
##***part001be;
sink()


##***partprelimplotb;
# Do the side-by-side dot plots and box-plots using ggplot2
plotprelim <- ggplot(cows, aes(x=Treatment, y=Weight))+
  geom_boxplot(alpha=0.2, notch=TRUE, outlier.shape=NA)+
  geom_jitter(size=4,position=position_jitter(width=0.2, height=0.1))+
  xlab("Treatment\n Point jittered to prevent overplotting")+
  ylab("Weight (lbs)")+
  ggtitle("Weight vs. Treatment with overlaid boxplots")
plotprelim
##***partprelimplote;

ggsave(plot=plotprelim, file="hormone-R-prelim.png",
       h=4,w=6, units="in", dpi=300) # save to a file


# Compute some summary statistics for each group 
sink('hormone-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(cows, "Treatment", summarize,
                n.cows     = length(Weight),
                mean.weight= mean(Weight),
                sd.weight  = sd(Weight))
report
# Hmm the report is not satisfactory because of missing values

# adjust the sample size and statisics for missing values
report <- ddply(cows, "Treatment", summarize,
                n.cows     = length(na.omit(Weight)),
                mean.weight= mean(Weight, na.rm=TRUE),
                sd.weight  = sd(Weight, na.rm=TRUE))
report

# a fancier report
report <- ddply(cows, "Treatment", sf.simple.summary, variable="Weight", crd=TRUE)
report
##***part003e;
sink()


# do the two sample t-test NOT assuming equal variances
sink('hormone-R-004.txt', split=TRUE)
##***part005b;
result <- t.test(Weight ~ Treatment, data=cows)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- abs(result$diff.in.means) / result$statistic
names(result$se.diff) <- 'SE.diff'
result
cat("Estimated difference  in means: ",result$diff.in.means,"\n")
cat("Estimated se for diff in means: ",result$se.diff,"\n")
##***part005e;
sink()


# do the two sample t-test  assuming equal variances
sink('hormone-R-005.txt', split=TRUE)
##***part006b;
result <- t.test(Weight ~ Treatment, data=cows, var.equal=TRUE)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- abs(result$diff.in.means) / result$statistic
names(result$se.diff) <- 'SE.diff'
result
cat("Estimated difference  in means: ",result$diff.in.means,"\n")
cat("Estimated se for diff in means: ",result$se.diff,"\n")
##***part006e;
sink()


#-----------------------------
# Use the lm() function which is equivalent to the two-sample t-test
# with equal variances

hormone.fit <- lm( Weight ~ Treatment, data=cows)
anova(hormone.fit)

# estimate the difference
hormone.fit.lsmo <- lsmeans::lsmeans(hormone.fit, ~Treatment)
cld(hormone.fit.lsmo)

pairs(hormone.fit.lsmo)
confint(pairs(hormone.fit.lsmo))
