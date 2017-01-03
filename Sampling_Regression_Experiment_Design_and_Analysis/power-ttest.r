# Power analysis for the weight vs treatment example with cows.
# 2015-04-16 CJS misc changes
# 2014-01-31 CJS Added parameter for significance level
#                Replaced for loop by plyr() function

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
library(ggplot2)
library(plyr)

# From the previous analyses, the 
#   standard deviation is around 100
#   the biologically significant difference in means is 50
#   the required power is .80
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.

# Compute the power of this design
sink("power-ttest-R-001.txt", split=TRUE)
##***part001b;
power <- power.t.test(delta=50, sd=100, power=.80, sig.level=0.05,
         type="two.sample",alternative="two.sided")
power
##***part001e;
sink()



# Look at the tradeoff between sample size and power when 
# the difference to detect is fixed.
##***part002b;
library(plyr)
my.results <- ldply(seq(2,100,2), function(n){
  # Compute the power for this value of n
  power <- power.t.test(delta=50, sd=100, n=n, sig.level=0.05,
              type="two.sample", alternative="two.sided")
  res <- c(n,power$power)
  names(res) <- c("n","power")
  res
})
my.results[1:10,]
##***part002e;

# now to plot
##***part003b;
plot002 <- ggplot(my.results,aes(x=n,y=power))+
     ggtitle("Power vs. sample size")+
     xlab("Sample size in each group")+
     ylab("Power")+ylim(0,1)+
     geom_line()+
     geom_hline(aes(yintercept=0.80))+ #ref line
     geom_vline(aes(xintercept=64))
plot002
##***part003e;
ggsave(plot=plot002, file="power-ttest-R-002.png", h=4, w=6, units="in", dpi=300)




