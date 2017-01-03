# Power analysis for a balance one-way ANOVA. THe cuckoo example is used.

# 2015-04-17 CJS Misc changes
#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

# From the previous analyses, the 
#   standard deviation is around 1 mm
#   a difference of 2 mm between the larges and smallest mean should be detected
#   so the set of expected means is: 21, 22, 22, 22, 22, 23 
#   the required power is .80
#   the significance level = 0.05
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.


sink('power-1fact-R-001.txt',split=TRUE)
##***part001b;
group.means <- c(21, 22, 22, 22, 22, 23)
sd <- 1 # what is the SD?
power <- power.anova.test(groups=length(group.means), 
         within.var=sd**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##***part001e;
sink()


sink('power-1fact-R-002.txt', split=TRUE)
##***part002b;
group.means <- c(21.1, 22.3, 22.6, 22.9, 23.1, 23.1)
sd <- 1 # what is the SD?
power <- power.anova.test(groups=length(group.means), 
         within.var=sd**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##***part002e;
sink()


