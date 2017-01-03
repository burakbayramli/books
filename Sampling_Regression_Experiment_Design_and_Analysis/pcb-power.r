# Power analysis for a balanced two-way CRD ANOVA. The PCB example
# The power computations will be approximate because the
# power.anova.test does not account for the loss of df in MSE
# due to the two factor model.

# 2015-07-15 CJS ##*** changed to ##***

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects

# From the previous analyses, the 
#   standard deviation is around 5 mm
#   the required power is .80
#   the significance level = 0.05
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.


sink('pcb-power-R-010.txt')
##***part010b;
# Power for a 4 unit diff in means between two sexes
group.means <- c(20, 24)
power <- power.anova.test(groups=length(group.means), 
         within.var=5**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##***part010e;
sink()


sink('pcb-power-R-020.txt')
##***part020b;
# Power for a 6 unit diff in means among 3 species
group.means <- c(10, 12, 16)
power <- power.anova.test(groups=length(group.means), 
         within.var=5**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power
##***part020e;
sink()
