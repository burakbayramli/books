# Single Mean with sub-sampling (pseudoreplication)

# This example is based on work conduced in the laboratory of 
# Prof. Lynn Quarmby at Simon Fraser University 
#  http://www.sfu.ca/mbb/People/Quarmby/. 
# Her research focus is on the the mechanism by which 
# cells shed their cilia (aka flagella) in response to stress working
# with the unicellular alga Chlamydomonas. 

# Microphotographs of the algae are taken, and the 
# length of one or two flagellum of each cell is measured.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

# Read in the data
sink('flagella1-R-001.txt')
##---part001b;
lengths <- read.csv("flagella1.csv", header=TRUE)
lengths[1:10,]
##---part001e;
sink()


#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the lengths for each cell
sink('flagella1-R-009.txt')
##---part009b;
avg <- apply(lengths[,c(2,3)],1,mean,na.rm=TRUE)
avg
##---part009e;
sink()

# Plot the data
png(file="flagella1-R-010.png")  # send the plot to a png file
##---part010b;
boxplot(avg,  ain="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data')
stripchart(avg, add=TRUE, 
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')
##---part010e;
dev.off()

sink("flagella1-R-011.txt")
##---part011b;
result <- t.test(avg)
names(result)
result$se.diff <- result$estimate / result$statistic
result
cat("SE of the average length is ", result$se.diff, "\n")
##---part011e;
sink()

# Add the confidence interval to the dot plot
png(file="flagella1-R-012.png")  # send the plot to a png file
##---part012b;
stripchart(avg,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')
abline(h=result$estimate, lty=3, lwd=3)
segments(.9, result$conf.int[1], .9,result$conf.int[2],
    lty=2, lwd=3)
##---part012e;
dev.off()


#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation

# We start by stacking the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
# on the rehape() function

sink('flagella1-R-020.txt')
##---part020b;
stack.length <- reshape(lengths, idvar="Cell...",
            varying=list(c("Flagellar.length.1","Flagellar.length.2")), 
            times=c("Flagellar.length.1","Flagellar.length.2"), timevar="Length",
            v.name="Length", 
            direction="long")
stack.length[1:10,]
##---part020e;
sink()

# Fit the random effects model
sink('flagella1-R-021.txt')
##---part021b;
library(lme4)
result2 <- lmer( Length ~ 1 + (1 |Cell...), data=stack.length)
summary(result2)
##---part021e;
sink()



