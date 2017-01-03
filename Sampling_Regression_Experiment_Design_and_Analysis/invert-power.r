# Power analysis for before vs. after study. 
# Invertebrate data from a simple before/after analysis.
# 2014-11-30 CJS make a before.after.power.stroup functions to parallel that for SLR
# 2014-11-21 CJS convert to ggplot; use plyr; split=TRUE added to sink()
# 2012-08-27 CJS First edition

#   Two streams were measured at a small hydro electric power project. At each stream
#   multiple quadrats were taken on the steam, and the number of invertebrates
#   was counted.

#   After 5 years, the hydro electric plant started up, and an additional 5 years
#   of data were collected.
 
#   Is there evidence of a change in abundance after the plant starts?

#   Note that this is NOT a BACI design, and is a VERY poor substitute for such.
#   The key problem is that changes may have occured over time unrelated
#   to the impact, so a "change" may simple be natural. 

#   Also, in this analysis we have ignored autocorrelation which can be
#   a serious problem in long-term studies. 

# The analysis of the data was done in the invert.sas program and
#   the variance components were extracted from program 

# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects
options(width=100)

# load the packages
library(plyr)
library(pwr)

source("../before-after-power-stroup.r")

# 1. Power analysis for a single stream before/after study 
#      based on the analysis of the averages.
#   Here the residual error is a combination of the year-to-year variation
#   and the quadrat-to-qaudrat variation based on a "average" number of quadrats
#   sampled each year. So the only thing that can change is the number of years before
#   or after impact occurred.
 
#   The power analysis is exactly the same as that for a two-sample t-test with 
#   the number of years before or after as the sample sizes
 
#  From the output the pooled estimate of variance is 6.7667**2 or a standard deviation of about 7

#  An illustration of how to compute power for a single scenario using the pwr package
pwr.t2n.test(n1=5, n2=5, d=10/6.7667, alternative="two.sided")

#  An illustration of how to use the before.after.power.stroup() function
#  Note that the X.after and X.before should have NO elements in common
#  Not unexpectedly you get the same answers as above
before.after.power.stroup(Shift=10, X.before=1:5, X.after=6:10, Process.SD=6.7667, Sampling.SD=0 )


sink("invert-power-R-100.txt", split=TRUE)
##***part100b;
# Note that d=effect size = difference to detect / standard deviation
power <- ldply(seq(5,20,1), function(n2){
     power1 <- pwr.t2n.test(n1=5, n2=n2, d=10/6.7667, 
               alternative="two.sided")$power
     power2 <- before.after.power.stroup(Shift=10, X.before=1:5, X.after=5+(1:n2), Process.SD=6.7667, Sampling.SD=0 )
     res <- c(n2=n2, power.pwr=power1, power.stroup=power2["power"])
     res     
})

cat("\n\nPower for detecting change in 10 in one stream with 5 years before and \n")
cat(    "different number of years after; average of 4.5 quadrats/year\n\n")
power
##***part100e;
sink()


# Egads - over 20 years post impact are required to detect a 10 unit change in density !!! 
# Perhaps we can improve things by changing the number of quadrats measured each years.
#   So we now need to separte the quadarat-to-quadrat and year-to-year variation.
#   We need to refer to the analysis on the individual values 


# If you are interested in detecting a percentage change, then you must convert
# both the size of the shift and the standard deviation to the proportional
# values. Choose a typical mean value when finding the proportions.

# For example, suppose that the Shift of 10 represents about a 20% change
# in a mean of around 50.
# The Process.SD and Sampling.SD must also be changed

Mean <- 50
Shift.prop      <- 10     /Mean
Process.SD.prop <- 6.7667 / Mean
Sampling.SD.prop<- 0      / Mean
Overall.SD.prop <- sqrt(Process.SD.prop^2 + Sampling.SD.prop^2)

pwr.t2n.test(n1=5, n2=5, d=Shift.prop/Overall.SD.prop, alternative="two.sided")

before.after.power.stroup(Shift=Shift.prop, X.before=1:5, X.after=6:10, 
                      Process.SD =Process.SD.prop, 
                      Sampling.SD=Sampling.SD.prop )






# 2. Power analysis for a single stream before/after study 
#      after changing the number of quadrats measured each year.
   
#   We need estimates of the quadrat-to-quadrat variation and the year-to-year variation.
 
#   The power analysis is the same as for a two-sample t-test with sub-sampling 
 
# From the analysis we find that 
#     residual variance = 59.17
#     year-to-year variance = 33.79 
# Note that the estimate of the resudal variance in the previous analysis of   
#      6.7667**2 = 45.78 is approx equal to  
#          33.79 (year-to-year variance) + 59.17 (quadrat-to-quadrat variation)/(4.5 or the average number of quadrats/year)

# We could use the previous method with different estimates of residual variance depending on the number of quadrats
#   measured per year. For example, suppose you could do 10 quadrats/year. Then the revised residual variance is approximately
#      33.79 + 59.17/10 = 39.707 and the residual std deviation is 6.30  


stddev <- sqrt(33.79 + 59.17/10)
pwr.t2n.test(n1=5, n2=5, d=10/stddev, alternative="two.sided")

# It is much easier to use the before.after.power.stroup function as you 
# don't have to compute the new standard deviation, but rather simply
# specify the process and sampling std deviation along with the approriate 
# number of replcates for X.before and X.after. Note that you don't have
# to have a balanced design with this function.
#  Note that the X.after and X.before should have NO elements in common
before.after.power.stroup(Shift=10, X.before=rep(1:5, each=10), X.after=rep(6:10, each=10), 
                          Process.SD=sqrt(33.79), Sampling.SD=sqrt(59.17) )



sink("invert-power-R-200.txt", split=TRUE)
##***part200b;
# Note that d=effect size = difference to detect / standard deviation
power <- ldply(seq(5,20,1), function(n2){
  power1 <- pwr.t2n.test(n1=5, n2=n2, d=10/stddev, 
                        alternative="two.sided")$power
  power2 <-before.after.power.stroup(Shift=10, 
                          X.before=rep(1:5, each=10), X.after=5+rep(1:n2, each=10), 
                          Process.SD=sqrt(33.79), Sampling.SD=sqrt(59.17) )

  res <- c(n2=n2, power.pwr=power1, power.stroup=power2["power"])
  res     
})
cat("\n\nPower for detecting change in 10 in one stream with 5 years before and \n")
cat(    "different number of years after; 10 quadrats/year\n\n")
power
##***part200e;
sink()


# Power has improved, but notice that even if you took infinite quadrats/year you can't reduce
#   the residual variance below that of the year-to-year variation.


# If you are interested in detecting a percentage change, then you must convert
# both the size of the shift and the standard deviation to the proportional
# values. Choose a typical mean value when finding the proportions.

# For example, suppose that the Shift of 10 represents about a 20% change
# in a mean of around 50.
# The Process.SD and Sampling.SD must also be changed

Mean <- 50
Shift.prop      <- 10     /Mean
Process.SD.prop <- sqrt(33.79) / Mean
Sampling.SD.prop<- sqrt(59.17) / Mean
Overall.SD.prop <- sqrt(Process.SD.prop^2 + Sampling.SD.prop^2)

pwr.t2n.test(n1=5, n2=5, d=Shift.prop/Overall.SD.prop, alternative="two.sided")

before.after.power.stroup(Shift=Shift.prop, X.before=1:5, X.after=6:10, 
                      Process.SD =Process.SD.prop, 
                      Sampling.SD=Sampling.SD.prop )



# Note that the previous power analysis is to detect a difference for THAT particular stream only 
#   and inference would be limited to that single stream only
 

# -----------------------------------------------------------

# 3. Power analysis for multiple streams before/after study 
#      based on changing the number of streams and number of quadrats measured per year.

# Method of Stroup not readily available in R but send me an email and I'll show you
# how to do this.

# -----------------------------------------------------------


# 4. Power analysis for multiple streams before/after study 
#      based on changing the number of streams and number of quadrats measured per year.
# 
#   Now inference is for the entire set of streams.
#   
#   We need estimates of the stream-to-stream, year-to-year, stream-year interaction variation, and the
#   quadrat-to-quadrat variation.
#  
#  From the analysis of multiple streams and individual values we find that 
#              Variance       StdDev        
#Year1         "3.664766e+01" "6.0537307199"
#Stream1       "3.831240e+01" "6.1897009333"
#Year1:Stream1 "4.063724e-07" "0.0006374734"
#Residual      "6.635497e+01" "8.1458562545"
#
# Notice that these differ slightly from those computed by SAS.

# You can use the Stroup method for power analysis if you want to change the number of years for each site etc
#   or you can use the previous methods if you assume that every site is measured every year and the same
#   number of quadrats is measured in each year-site combination
# I have not implemented this method in R yet.
 
# Alternatively, if the design is balanced, we can use the previous results by computing
#   the residual variance as
#     year-to-year variation 
#     year-stream interaction variation / n_stream
#     quadrat variance = n_streams*n_quadrates
#   Note that because the same site is measured over time, it "disappears" from the power computation
#   in the same way as blocking causes block effects to disappear.

# 
#   
#   So for the case of 4 streams and 5 quadrats/stream/year, our estimated residual variation is
#     36.65 + 0/4 + 66.35/4/5 = 39.96 and the std dev is sqrt(39.96)=6.32
#    

# Once again notice that the limiting term for the power analysis is the year-to-year variation
#   and that even if you measure thousands of streams, you can't ever reduce the impact of this term.

# Note that for a single stream, the "year-to-year" variance really is a combination of 
# the year-to-year variance and the year-stream interaction variance components as these
# cannot be separated if you have a single stream.
 
# It appears that multiple-streams have no impact vs. a single stream, but this is misleading. As noted
# earlier, with a single stream, you cannot separate the year-to-year variance and the stream-year
# interaction variance components. With multiple streams, you can separate the two components and 
# reduce the impact of the stream-year interaction by measuring multiple streams.  

 
#   Additionally, the scope of inference is different. In the case of a single stream, you are interested 
#   ONLY in that stream. In the case of
#   multiple streams, you want to generalize to all streams. In this case, the stream-year interaction
#   variance was estimated to be fzero, so this implies that there is no evidence that individual streams
#   behave differently from each other. If this variance component was non-zero, then you have to deal
#   with the additional variation caused by individual streams behaving differently over time from each other. 
 
sink("invert-power-R-400.txt", split=TRUE)
##***part400b;
year_var        <- 36.64
stream_var      <- 38.3  # not needed
year_stream_var <- 0
quad_var        <- 66.35

# Note that d=effect size = difference to detect / standard deviation
power                <- data.frame(n_after=5:20)
power$n_before        <- 5
power$year_var        <- year_var
power$year_stream_var <- year_stream_var
power$quad_var        <- quad_var
power$n_stream        <- 4
power$n_quad          <- 5
power$alpha           <- .05
power$diff            <- 10
powerres <- adply(power,1,function(x){
  # compute power for one row of the table
  stddev <- sqrt(x$year_var + x$year_stream_var/x$n_stream +
                 x$quad_var/x$n_stream/x$n_quad)
  power <- pwr.t2n.test(n1=x$n_before, 
                        n2=x$n_after, 
                        d =x$diff/stddev, 
                       alternative="two.sided")$power
  names(power) <- "power"
  return(power)
})

cat("\n\nPower for detecting change in 10 in 4 streams with 5 years before and \n")
cat(    "different number of years after; 5 quadrats/year\n\n")
cat("\n\nNote that power is now for a larger scope of inference \n\n")
powerres
##***part400e;
sink()
