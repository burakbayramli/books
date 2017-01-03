# Fat content and tumor weights
# 2015-04-16 CJS typo fixes; add size to ggsave; removed extra dots from box plots
# 2014-05-17 CJS scrap Base R graphics; scrap doBy package; use url for schwarz functions
# 2014-04-19 CJS ggplot, lsmeans, etc

# Does the amount of fat in a diet influence the weight of tumors?
# Rats were randomly assigned to a low fat or a high fat diet and at the
# end of the experiment, the weight of tumors was recorded.
#

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case internet not available
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)

sink('fattumor-R-000.txt', split=TRUE)
# Read in the data
##***part001b;
rats <- read.csv('fattumor.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
rats$Diet <- as.factor(rats$Diet)
rats[1:12,]
##***part001e;
sink()

# Get side-by-side dot plots


##***part002b;
# make the plot using ggplot (turn off outliers because we alreay have points plotted)
plotprelim <- ggplot(data=rats, aes(x=Diet, y=TumorWeight))+
  geom_boxplot(alpha=0.2, notch=TRUE, outlier.shape=NA)+ # don't repeat outlier points
  geom_jitter(size=4, position=position_jitter(width=0.1, height=0.1))+
  xlab("Diet\n Point jittered to prevent overplotting")+
  ylab("Tumor weight (g)")+
  ggtitle("Tumor Weight vs. Diet with overlaid boxplots")
plotprelim
##***part002e;

ggsave(plotprelim, file="fattumor-R-prelim.png", h=4, w=6, units="in", dpi=300)

# Compute some summary statistics for each group
sink('fattumor-R-002.txt', split=TRUE)
##***part003b;
report <- ddply(rats, "Diet", summarize,
                  n.rats     = length(TumorWeight),
                  mean.weight= mean(TumorWeight),
                  sd.weight  = sd(TumorWeight))
report
report <- ddply(rats, "Diet", sf.simple.summary, variable="TumorWeight", crd=TRUE)
report
##***part003e;
sink()

# do the two sample t-test not assuming equal variances
sink('fattumor-R-004.txt', split=TRUE)
##***part005b;
result <- t.test(TumorWeight ~ Diet, data=rats)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- abs(result$diff.in.means) / result$statistic
names(result$se.diff) <- 'SE.diff'
result
cat("Estimated difference  in means:",result$diff.in.means,"\n")
cat("Estimated se for diff in means:",result$se.diff,"\n")
##***part005e;
sink()


# do the two sample t-test  assuming equal variances
sink('fattumor-R-005.txt', split=TRUE)
##***part006b;
result <- t.test(TumorWeight ~ Diet, data=rats, var.equal=TRUE)
result$diff.in.means <- sum(result$estimate*c(1,-1))
names(result$diff.in.means)<- "diff.in.means"
result$se.diff <- abs(result$diff.in.means) / result$statistic
names(result$se.diff) <- 'SE.diff'
result
cat("Estimated difference  in means:",result$diff.in.means,"\n")
cat("Estimated se for diff in means:",result$se.diff,"\n")
##***part006e;
sink()

#-----------------------------
# Use the lm() function which is equivalent to the two-sample t-test
# with equal variances

diet.fit <- lm( TumorWeight ~ Diet, data=rats)
anova(diet.fit)

# estimate the difference
diet.fit.lsmo <- lsmeans::lsmeans(diet.fit, ~Diet)
cld(diet.fit.lsmo)

pairs(diet.fit.lsmo)
confint(pairs(diet.fit.lsmo))


