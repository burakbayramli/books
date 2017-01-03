# Power analysis for the stream slope example
# 2014-04-20 CJS ggplot etc
#
# Lines starting with ##---part001b; or ##---part001e; bracket the source 
# line for inclusing by LaTex and usually are not coded.
#

library(ggplot2)
library(plyr)

#**************************************************************
# Method 1. Think about the difference in density.

# From the previous analyses, the 
#   standard deviation of the DIFFERENCES is around 5.701
#   the biologically significant difference in means is 2
#   the required power is .80
# We wish to solve for n, the sample size, so that argument
# is omitted in the call.

sink('power-stream-R-001.txt', split=TRUE)
##---part001b;
power <- power.t.test(delta=2, sd=5.701, power=.80,
         type="paired",alternative="two.sided")
power
##---part001e;
sink()

# Look at the tradeoff between sample size and power when 
# the difference to detect is fixed.
##---part002b;
my.results <- ldply(seq(2,100,2), function(n){
  # compute the power at this value of n and return a vector
  power <- power.t.test(delta=2, sd=5.701, n=n,
              type="paired", alternative="two.sided")
  res<- c(power$n,power$power)
  names(res) <- c("n","power")
  res
  })
my.results[1:10,]
##---part002e;

##---part003b;
# now to plot
plot001 <- ggplot(data=my.results, aes(x=n, y=power))+
  ggtitle("Power vs. sample size")+
  ylab("Power")+xlab("Sample size in EACH group")+
  geom_path()+
  geom_hline(yintercept=0.80)+
  geom_vline(xintercept=64)
plot001
##---part003e;

ggsave(plot=plot001, file='power-stream-R-002.png')

# same plot using Base R graphics
plot(my.results[,"n"],my.results[,"power"], type="l",
   main="Power vs sample size",
   xlab="Sample size in each group",
   ylab="Power")
abline(h=.80) # add reference lines
abline(v=64)





