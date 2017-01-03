#   Change point problem.
#  2015-04-27 CJS added latest data
#  2014-11-28 CJS added power computations to end
#  2014-11-27 CJS ggplot; **--- problem; sink; change point

#  The Nenana river in the Interior of Alaska usually freezes 
#  over during October and November. The ice continues to grow throughout 
#  the winter accumulating an average maximum thickness of about 110 cm, 
#  depending upon winter weather conditions. The Nenana River Ice Classic 
#  competition began in 1917 when railroad engineers bet a total of 800 dollars, 
#  winner takes all, guessing the exact time (month, day, hour, minute) ice 
#  on the Nenana River would break up. Each year since then, Alaska 
#  residents have guessed at the timing of the river breakup. A tripod, 
#  connected to an on-shore clock with a string, is planted in two feet 
#  of river ice during river freeze-up in October or November. 
#  The following spring, the clock automatically stops when the tripod 
#  moves as the ice breaks up. The time on the clock is used as the river 
#  ice breakup time. Many factors influence the river ice breakup, 
#  such as air temperature, ice thickness, snow cover, wind, water 
#  temperature, and depth of water below the ice. Generally, the Nenana 
#  river ice breaks up in late April or early May (historically, April 20 to May 20). 
#  The time series of the Nenana river ice breakup dates can be used to 
#  investigate the effects of climate change in the region.

#  In 2010, the jackpot was almost $300,000 and the ice went out at 
#  9:06 on 2010-04-29. In 2012, the jackpot was over $350,000 
#  and the ice went out at 19:39 on 2012-04-23 - 
#  as reported at http://www. cbc.ca/news/offbeat/story/2012/05/02/alaska-ice-contest.html. 
#  The latest winner, Tommy Lee Waters, has also won twice before, 
#  but never has been a solo winner. Waters spent time drilling holes in 
#  the area to measure the thickness of the ice. Altogether he spent 
#  $5,000 on tickets for submitting guesses (he purchased every minute of the 
#  afternoon of 23 April) and spent an estimated 1,200 hours working out 
#  the math by hand. And, it was also his birthday! (What are the odds?) 
#  You too can use statistical methods to gain fame and fortune!

#  More details about the Ice Classic are available at 
#  http://www.nenanaakiceclassic.com.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(SiZer) # load package with reference to Toms and Lesperance (2003)

# Read in the data
sink('nenana-R-010.txt', split=TRUE)
##***part010b;
nenana <- read.csv("nenana.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
nenana[1:10,]
##***part010e;
sink()


# Create a plot of the raw data
# after fitting a linear model
lmfit <- lm(Julian.Date ~ Year, data=nenana)

plotdata <- ggplot(data=nenana, aes(x=Year, y=Julian.Date))+
  ggtitle("Nenana Ice Breakup")+
  geom_point(size=3)+
  geom_abline(intercept=coef(lmfit)[1], slope=coef(lmfit)[2])
plotdata




# Add a variable for a known change point of 1970
##***part011b;
nenana$cp1970p <- pmax(0, nenana$Year-1970)
nenana$Year
nenana$cp1970
##***part011e;

sink("nenana-R-020.txt", split=TRUE)
##***part020b;
# Fit the known change point model
cp1970.fit <- lm(Julian.Date ~ Year +cp1970p, data=nenana)
summary(cp1970.fit)
##***part020e;
sink()


# Plot the broken stick model
plot2 <- plotdata +
  ggtitle("Nenena Ice Breakup - with break point at known 1970 added")+
  geom_line(aes(y=predict(cp1970.fit)), color="red", linetype="dashed", size=3)
plot2
ggsave(plot=plot2, file="nenana-R-cp1970.png", h=4, w=5, units="in", dpi=300)



sink('nenana-R-040.txt', split=TRUE)
##***part040b;
# Fit the a broken stick to the Julian Date
pw.model <- piecewise.linear(nenana$Year, nenana$Julian.Date, middle=1, CI=TRUE,
                             bootstrap.samples = 1000, sig.level = 0.05)
pw.model
##***part040e;
sink()


# Add the unknown breakpoint line to the previous plot
plot3 <- plot2 +
  ggtitle("Nenana Ice Breakup - two breakpoint models addeed")+
  geom_line(aes(y=predict(pw.model, nenana$Year)), color="blue", linetype="dotdash", size=2)
plot3
ggsave(plot=plot3, file='nenana-R-040.png', h=4, w=6, units="in", dpi=300)


###################################################################################
# Power analyses
# Because we have a single measurement per year, process and sampling error
# are completely confounded together and cannot be separated.

# Get the power function
source("../../Power/RegPower/SLR-Power/slr-power-stroup.r")

# (a) on the absolute trend scale
# Here we want to determine the number of years needed to detect a trend of 1 day/year decrease in average time of breakup
# duration

# We estimte the process+sampling SD from the previous fit
Process.SD  <- summary(lmfit)$sigma
Sampling.SD <- 0
Trend       <- 1

Xvalues <- 1:10 # 10 years of yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


Xvalues <- 1:20 # 20 years of yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


Xvalues <- seq(1,20,2) # 20 years of bi-yearly sampling
slr.power.stroup( Trend=Trend, Xvalues=Xvalues,   # evenly spaced data
                 Process.SD=Process.SD, Sampling.SD=Sampling.SD, alpha=0.05)


# (b) a proportion change (percentage change) per year
# Because the Y variable (Julian.Date) is an INTERVAL scale (no natural zero)
# vs. the usual RATIO scale, it makes NO sense to think about %changes/year.

# An INTERVAL scaled variable has NO Natural zero -- the Julian date is computed starting
# from 1 January, but the same trend would be found if the dates of breakup were counted
# starting from 1 Feb, or 1 March or eveh 1 July (you would have negative times of breakup!)



