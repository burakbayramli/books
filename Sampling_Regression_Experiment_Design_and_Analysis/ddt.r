# Illustration of how to separate process and sampling error using simulated data.
# 2014-07-29 CJS First Edition


# Generate DDT data with year-to-year random variation and within bird random variation.
#

library(ggplot2)
library(lme4)
library(plyr)

set.seed(23432)
mean.DDT   <- 100
process.SD <- 10
sample.SD  <- 5
sample.size <- data.frame(year=1:5, birds=c(10,5,10,5,10))

gen.data <- ddply(sample.size, "year", function(x,mean, process.SD, sample.SD){
  # Generate one year of data
  ddt <- mean + rnorm(1,0,process.SD) + rnorm(x$birds,0,sample.SD)
  ddt <- round(ddt,1)
  res <- data.frame(year=x$year, ddt=ddt)
  return(res)
}, mean=mean.DDT, process.SD=process.SD, sample.SD=sample.SD)

write.csv(gen.data, 'ddt.csv', row.names=FALSE)



ddtplot <- ggplot(data=gen.data, aes(x=year, y=ddt))+
   ggtitle("Process and sampling error - simulated data")+
   geom_point()+
   geom_hline(yintercept=mean(gen.data$ddt))
ddtplot
ggsave(plot=ddtplot, file="ddt-plot-001.png", height=4, width=6, units="in")

# Fit a model to measure the two sources of variation.
gen.data$yearF <- factor(gen.data$year) # convert this to factor
library(lme4)
fit <- lmer( ddt ~ 1 + (1|yearF), data=gen.data)
summary(fit)
sink("ddt-varcomp-R-001.txt", split=TRUE)
VarCorr(fit)  # Extract the variance components
sink()
