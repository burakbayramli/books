###########################
# Searching for the time where treatment and control diverge
# Change point problem.

# 2014-11-28 CJS ggplot; ##*** problem; 

# This example is based on a project by Nate Derstine of 
# Biological Sciences at Simon Fraser University.
# The data are simulated, but illustrative of the process.

# Considered one of the worst invasive pest ants, the electric ant, 
# or little fire ant ({\it Wasmannia auropunctata} (Roger) (Hymenoptera:
# Formicidae)) has negatively impacted both biodiversity and agriculture. 
# Its distribution is nearlypantropical, and greenhouse infestations have 
# been reported as far north as Canada and the United
# Kingdom. Current {\it W. auropunctata} detection methods commonly employ a food item like peanut
# butter. An alternative detection method may be found in pheromone attractants. For W. auropunctata,
# a one-way trap containing an alarm pheromone has been
# successfully used to detect little fire ant populations in macadamia nut orchards. What is the
# longevity of this type of pheromone lure as used in a unique one-way ant
# trap.

# At the beginning of the experiment, 180 control traps and 180 treatment traps were prepared. On each day,
# three traps of each type were randomized to locations in the orchard where the ant species were known to be present.
# 24 hours later, the traps were retrieved and the number of ants capture counted and the trap is discarded.


options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(plyr)
library(SiZer) # broken stick models



# Read in the data
sink('ant-R-010.txt', split=TRUE)
##***part010b;
ant <- read.csv("ant.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
ant$trt  <- factor(ant$trt)
ant[1:10,]
##***part010e;
sink()

str(ant)

##***part020b;
# Create a plot of the raw data
prelimplot <- ggplot(data=ant, aes(x=day, y=count, group=trt, color=trt, shape=trt))+
  ggtitle("Number of ants captured in traps")+
  geom_point()
prelimplot
##***part020e;
ggsave(plot=prelimplot, file='ant-R-020.png', h=4, w=6, units="in", dpi=300)


##***part022b;
# Do a t-test on each day's counts and plot the p-value
day.test <- ddply(ant, "day",  function(x){
               temp <- t.test(count~trt,data=x); 
               data.frame(pvalue=temp$p.value)})
##***part022e;


##***part023b;
pvalueplot <- ggplot(data=day.test, aes(x=day, y=pvalue))+
  ggtitle("Naive analysis - p-values of individual t-tests")+
  geom_point()+
  geom_hline(yintercept=0.05)
pvalueplot
##--part023e;
ggsave(plot=pvalueplot, file='ant-R-023.png', h=4, w=6, units="in", dpi=300)




##***part030b;
# Compute the mean for each day and treatment 
library(doBy)
mean_count <- ddply(ant, c("day","trt"), function(x){data.frame(count.mean=mean(x$count))}) 
head(mean_count)
##***part030e;



##***part031b;
meanplot <- ggplot(data=mean_count,aes(x=day, y=count.mean, group=trt, color=trt, shape=trt))+
  ggtitle("Mean count by day")+
  geom_point()
meanplot
##***part031e;
ggsave(plot=meanplot, file="ant-R-031.png", h=4, w=6, units="in", dpi=300)



##***part032b;
# Find the log(ratio) between the mean counts
# We need to reshape to get both means on same line
mean_count2 <- reshape(mean_count, v.names="count.mean", 
                timevar="trt", direction="wide", idvar="day")
mean_count2$log_ratio <- with(mean_count2,
              log(count.mean.Treatment/count.mean.Control))
ratioplot <- ggplot(data=mean_count2,aes(x=day, y=log_ratio))+
  ggtitle("log(ratio of means Treatment/Control) by day")+
  ylab("log( Treatment/Control mean counts")+
  geom_point()+
  geom_hline(yintercept=0)
ratioplot
##***part032e;
ggsave(plot=ratioplot, file='ant-R-032.png', h=4, w=6, units="in", dpi=300)


sink('ant-R-040.txt', split=TRUE)
##***part040b;
# Fit the a broken stick to the log(ratio of means)

pw.model <- piecewise.linear(mean_count2$day, mean_count2$log_ratio, middle=1, CI=TRUE,
                             bootstrap.samples = 1000, sig.level = 0.05)
pw.model
##***part040e;
sink()


ratioplot2 <- ratioplot+
  geom_line(aes(y=predict(pw.model, mean_count2$day)))
ratioplot2
ggsave(plot=ratioplot2, file='ant-R-040.png', h=4,w=6,  units="in",dpi=300)



# Why was the changepoint estimated so poorly.
# The problem is that there are no constraints on the slope after the
# break in the line. So a model such as:
pw.model.newcp <- lm(log_ratio ~ day + pmax(0,day-30), data=mean_count2)
pw.model.newcp.predict <- predict(pw.model.newcp, newdata=data.frame(day=1:60))

ratioplot3 <- ratioplot2 +
  geom_line(aes(y=pw.model.newcp.predict, x=1:60), color="blue", linetype='dashed')+
  xlab("Day \n blue dashed is change point at day 30")
ratioplot3
ggsave(plot=ratioplot3, file='ant-R-041.png', h=4, w=6, units="in", dpi=300)



# Broken stick with second half forced to be zero.
# First load the revised functions
source("changepoint0.r")

sink('ant-R-050.txt', split=TRUE)
##***part050b;
# Do the fit with a 0 slope after the change point
pw.model0 <- piecewise.linear0(mean_count2$day, mean_count2$log_ratio, middle=1, CI=TRUE,
                               bootstrap.samples = 1000, sig.level = 0.05)
pw.model0
##***part050e;
sink()

# We can't use the default plot method and must create the explicit plot
pw.model0.df <- data.frame(days=pw.model0$x, log_ratio=pw.model0$y)

ratioplot4 <- ratioplot3 +
   geom_line(data=pw.model0.df, aes(x=days, y=log_ratio) , color="red", size=2, type="dotdashed")+
   xlab("Day \n blue dashed is change point at day 30 \n red dashed in change point with 0 after break")
ratioplot4             
ggsave(plot=ratioplot4, file='ant-R-050.png', h=4, w=6, units="in", dpi=300)                   
                  



