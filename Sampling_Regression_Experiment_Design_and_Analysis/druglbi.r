# Weight loss after different treatments
# This is the DrugLBI file that ships in the JMP sample data
# library.

# This will illustrate various features of R and
# how to do basic statistics and inference.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects


# Get the data into R
# This creates a data frame (a bound list of variable) with
# only one variable (LBS).

sink('druglbi-R-001.txt')
##---part001b;
study <- read.csv("druglbi.csv", header=TRUE)
study
##---part001e;
sink()

# Do a preliminary plot
# With only a few data points the plots are not that
# interesting. This also illustrates how to
# save plots to an external file

png('druglbi-R-002.png')
##---part002b;
layout(matrix(1:2,2,1))
plot(study$LBS, main="LBS by order of reading")
boxplot(study$LBS ~ study$Drug, main="Box plot")
##---part002e;
dev.off()


# Gather some summary statistics on the data.
# We use the length, mean, and sd function as well as
# The summaryBy() function from the doBy library.
sink('druglbi-R-003.txt')
##---part003b;
library(doBy)

report <- summaryBy(LBS ~Drug, data=study, FUN=c(length,mean,sd))
cat(" A summary report of the LBS data is\n")
report
##---part003e;
sink()


# Find the estimated se of the mean.
# Notice how we add a new variable to the report data frame
sink('druglbi-R-004.txt')
##---part004b;
report$se.LBS.mean <- report$LBS.sd/sqrt(report$LBS.length)
cat(" A summary report of the LBS data is\n")
report
##---part004e;
sink()


# Find the confidence interval for the population mean
# for each group. Note the use of the tapply function


sink('druglbi-R-005.txt')
##---part005b;
lower.ci <- tapply(study$LBS, study$Drug,
    function(v) t.test(v)$conf.int[1])
upper.ci <- tapply(study$LBS, study$Drug,
    function(v) t.test(v)$conf.int[2])
cbind(lower.ci, upper.ci)
##---part005e;
sink()

# Plot the confidence interval on the box plot

png('druglbi-R-006.png')
##---part006b;
library(gplots)
layout(matrix(1:2),2,1)
stripchart(study$LBS~study$Drug, vertical=TRUE,method="jitter")
plotCI(report$LBS.mean,lwd=3, 
     ui=upper.ci, 
     li=lower.ci,
     add=TRUE)
boxplot(study$LBS~study$Drug)
plotCI(report$LBS.mean,lwd=3, 
     ui=upper.ci, 
     li=lower.ci,
     add=TRUE)
##---part006e;
dev.off()


