# Potato Peeling Times
# 2015-04-17 CJS remove doby; misc changes
# 2014-04-18 CJS ggplot, lsmeans etc

# This looks at the time to peel potatos using a 
# peeler, a knife, or a peeler with the opposite hand.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


sink('potato-R-000.txt', split=TRUE)
# Read in the data
##***part001b;
peeling <- read.csv('potato.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
peeling$group <- as.factor(peeling$group)
peeling
##***part001e;
sink()

str(peeling)
##***part002b;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=peeling, aes(x=group, y=time))+
  ggtitle("Times to peel potatos")+
  xlab("Group")+ylab("Time (min)")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)
plotprelim
##***part002e;

ggsave(plot=plotprelim, file='potato-R-prelim.png', h=4, w=6, units="in", dpi=300)


# Compute some summary statistics for each group using my special functions
sink('potato-R-002.txt', split=TRUE)
##***part003b;

report <- ddply(peeling, "group", summarize,
                n.test     = length(time),
                mean.time= mean(time),
                sd.time  = sd(time))
report

report <- ddply(peeling, "group", sf.simple.summary, variable="time", crd=TRUE)
report
##***part003e;
sink()




# fit the linear model and get the ANOVA table and test for effects
sink('potato-R-004.txt', split=TRUE)
##***part005b;
result <- lm(time ~ group, data=peeling)
anova(result)
##***part005e;
sink()

##***part006bb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***part006be;

ggsave(plot=plotdiag, file='potato-R-diag.png',h=4,w=6, units="in", dpi=300)




# Now to estimate the marginal means and do a multiple comparison procedure
result.lsmo <- lsmeans::lsmeans(result, ~group, adjust='tukey')

# Get the individual means
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)

cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld

# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="group")
plotcld <- plotcld + 
        xlab("group")+
        ylab("Mean time (with 95% ci")+
        ggtitle("Comparison of mean time with cld")
plotcld
ggsave(plot=plotcld, file='potato-R-cldbar.png',h=4, w=6, units="in", dpi=300)


# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="group")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("group")+
        ylab("Mean time (with 95% ci")+
        ggtitle("Comparison of mean time with cld")
plotcldb
ggsave(plot=plotcldb, file='potato-R-cldline.png',h=4,w=6, units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

# Find all the pairwise differences adjusting for multipicity
result.pairs <- pairs(result.lsmo, adjust='tukey')
summary(result.pairs, infer=TRUE)

# Make a plot of the differences
result.pairs.ci <- confint(result.pairs) # extract the ci values
result.pairs.ci
plotdiff <- ggplot(result.pairs.ci, aes(contrast, estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_point(size=4)+
    geom_linerange(size=1.5)+
    geom_abline(interecept=0, slope=0, linetype=2)+
    ylab("Estimated diff and 95% ci")+
    xlab("Contrast")+
    ggtitle("Estimated pairwise differences and ci")
plotdiff
ggsave(plot=plotdiff, file='potato-R-pairdiff.png',h=4, w=6, units="in", dpi=300)




