# Power analysis for a balanced RCB. THe herbicide example.
# This is only an approximation as no accounting for the loss of
# degrees of freedom in the MSE has been applied. This usually
# should not be a problem.

# 2014-04-20 CJS ggplot  etc
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects


# From the previous analyses, the 
#   standard deviation is around .174 based on the average
#            standard deviation in each group or the sqrt(MSE) from the initial
#            ANOVA
#   a difference of 0.2 between the larges and smallest mean should be detected
#   so the set of expected means is: 0, .1, .2
#   the required power is .80
#   the significance level = 0.05
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.


sink('herbicide-power-R-001.txt', split=TRUE)
##---part001b;
group.means <- c(0, .1, .2)
my.sd  <- .174
power <- power.anova.test(groups=length(group.means), 
         within.var=my.sd**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##---part001e;
sink()

