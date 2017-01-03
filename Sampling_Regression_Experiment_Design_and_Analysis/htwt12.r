# Height of children.
# 2015-04-18 CJS remove base R graphics; clean up; remove doby; 
# 2014-01-30 CJS split=TRUE, as.is/strip.white/plyr

# A study of height and weight measurements from 63 children, all age 12. 
#  Is it safe to say at age 12, the mean height for males will be greater than for females?

# This is the HTWT12 datafile in the JMP sample data library.
#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case of no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


# Read in the data
sink('htwt12-R-000.txt', split=TRUE)
##***part001b;
htwt12 <- read.csv('htwt12.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
htwt12$Gender <- factor(htwt12$Gender)
htwt12[1:12,]
##***part001e;
sink()

# Get side-by-side dot plots
##***part002b;
plotprelim <- ggplot(data=htwt12, aes(x=Gender, y=Height))+
    ggtitle("Height vs. Gender at 12 years of age")+
    xlab("Gender")+ ylab("Height (in)")+
    geom_point(size=4, position=position_jitter(height=0.1, width=0.1))+
    geom_boxplot(alpha=0.2, notch=TRUE, outlier.shape=NA)
plotprelim
##***part002e;
ggsave(plot=plotprelim, file='htwt12-R-prelim.png', h=4, w=6, units="in", dpi=300)



# The easiest way to make a nice report showing summary
# statistics by group in R is with plyr() package
sink('htwt12-R-002.txt', split=TRUE)
##***part003b;
report <- ddply(htwt12, "Gender", summarize,
                n.cild     = length(Height),
                mean.weight= mean(Height),
                sd.weight  = sd(Height))
report

report <- ddply(htwt12, "Gender", sf.simple.summary, variable="Height", crd=TRUE)
report
##***part003e;
sink()



# do the two sample t-test not assuming equal variances
sink('htwt12-R-004.txt', split=TRUE)
##***part005b;
result <- t.test(Height ~ Gender, data=htwt12)
# compute the se based on t.value = estimated diff/ se(diff )
names(result)
result$diff <- sum(c(1,-1)*result$estimate)
result$se.diff <- sum(result$estimate*c(1,-1)) /result$statistic
names(result$se.diff)<- "SE.diff"
result
cat("Estimated difference in means is ",result$diff,
    "   SE( ",result$se.diff,")\n")
##***part005e;
sink()


# do the two sample t-test  assuming equal variances
sink('htwt12-R-005.txt', split=TRUE)
##***part006b;
result <- t.test(Height ~ Gender, data=htwt12, var.equal=TRUE)
result$diff <- sum(c(1,-1)*result$estimate)
result$se.diff <- sum(result$estimate*c(1,-1)) /result$statistic
names(result$se.diff)<- "SE.diff"
result
cat("Estimated difference in means is ",result$diff,
    "   SE( ",result$se.diff,")\n")
##***part006e;
sink()


#-----------------------------
# Use the lm() function which is equivalent to the two-sample t-test
# with equal variances

gender.fit <- lm( Height ~ Gender, data=htwt12)
anova(gender.fit)

# estimate the difference
gender.fit.lsmo <- lsmeans::lsmeans(gender.fit, ~Gender)
cld(gender.fit.lsmo)

pairs(gender.fit.lsmo)
confint(pairs(gender.fit.lsmo))

