######################## © CSIRO Australia 2005 ###############################
# Session 01:  Whirwind Tour of R                                             #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

# Exploratory Analysis: Whiteside Gas Consumption Data
require(MASS)
data(whiteside)		# not necessary with MASS data sets
find(whiteside)
names(whiteside)
require(lattice)

graphics.off()			# not necessary but often useful
trellis.par.set(col.whitebg()) 	# make a better choice of colours.
xyplot(Gas ~ Temp | Insul, whiteside,
  xlab = "Average external temperature (deg. C)",
  ylab = "Gas consumption (cu.ft/1000)",
  as.table = T, aspect = 1, layout = c(2,1),
  ylim = range(0, whiteside$Gas+0.5),
  xlim = range(0, whiteside$Temp+0.5),
  panel =
    function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  })

#############################
# Modelling: Whiteside Gas Consumption Data
# Fit straight lines separately to "before" and "after"
gasB <- lm(Gas ~ Temp, whiteside,subset = Insul == "Before")
gasA <- update(gasB, subset = Insul=="After")
summary(gasB)
summary(gasA)
# Both lines in the one fitted model
gasBA <- lm(Gas ~ Insul/Temp - 1, whiteside)
summary(gasBA)
# Check for curvature
gasQ <- lm(Gas ~ Insul/(Temp + I(Temp^2)) - 1, 	whiteside)
summary(gasQ)$coef
# Test for parallelism
gasPR <- lm(Gas ~ Insul + Temp, whiteside)
anova(gasPR, gasBA)
# Residual Checks
res <- resid(gasBA)
fit <- fitted(gasBA)
plot(fit, res,xlab="Fitted Values",ylab="Residuals",main="Plot of Residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(res)
qqline(res)
title(main="Normal Q-Q Plot")

##########################
# Exploratory Analysis:  Cars and Fuel Economy
require(MASS)
Cars93[1:3,]
# Scatterplot of the data
# attach graphics library
require(lattice)
# graphics setup
trellis.par.set(theme=col.whitebg())
# plotting
with(Cars93,xyplot(100/MPG.highway~Weight,
   ylab="Gallons per 100 miles",
   main="Cars(1993 Makes & Models)",pch=16))
   
###########################
# Modelling:  Cars and Fuel Economy
with(Cars93,xyplot(100/MPG.highway~Weight,
	   ylab="Gallons per 100 miles",pch=16,
		  main="Cars(1993 Makes & Models)",
	   panel =
	      function(x, y, ...) {
	         panel.xyplot(x, y, ...)
	         panel.lmline(x, y,col="red", ...)
      }))
      
Cars93 <- transform(Cars93,GPM.highway = 100/MPG.highway,
    WeightT=Weight/1000)
cars93.lm <- lm(GPM.highway~Weight,data=Cars93)
summary(cars93.lm)
# Residual Checks
# set up plotting region
par(mfrow=c(2,2))
# residual plot
plot(fitted(cars93.lm),resid(cars93.lm),xlab="Fitted Values",ylab="Residuals",
		main="Residual Plot")
abline(h=0,lty=2)
# Normal Q-Q plot of residuals
qqnorm(resid(cars93.lm),main="Normal Q-Q Plot of Residuals")
qqline(resid(cars93.lm))
# Histogram of residuals
frame()
hist(resid(cars93.lm),main="Histogram of Residuals")
par(mfrow=c(1,1))

#############################
# Relationship between mileage, weight and type
with(Cars93,xyplot(GPM.highway~Weight|Type, ylab="Gallons per 100 miles",
		pch=16,main="Cars(1993 Makes & Models)"))
# Model fits and first results
require(nlme)
Cars93.lme1 <- lme(GPM.highway ~ Type + WeightT, data=Cars93,
  random= ~1 | Manufacturer)
round(summary(Cars93.lme1)$tTable, 4)
re <- ranef(Cars93.lme1)
o <- order(re[, 1])
re[o,,drop = F]
summary(Cars93.lme1)
# Significance of fixed effects terms
Cars93.lme2 <- update(Cars93.lme1,method = "ML")
dropterm(Cars93.lme2, test="Chisq")

#########################
# Prices of Cars (1993 Makes and Models)
# Price split by manufacturer
oldpar <- par("mar")
par(mar=c(8,4.1,4.1,2.2))
with(Cars93,boxplot(split(Price,Manufacturer),las=2,ylab="Price (in $1,000)"))
par(mar=oldpar)

#############################
# Images of Volcanic Activity
# Using the image function
image(1:nrow(volcano),1:ncol(volcano),volcano,col=heat.colors(12),
  xlab="",ylab="")
title(main="image()")
# Now with user defined graduated colors
require(grDevices)
cols <- gplots::colorpanel(25,"blue","yellow","red")
fr <- cut(volcano,breaks=quantile(volcano,0:25/25),include=T)
plot(row(volcano),col(volcano),pch=15,col=cols[fr],cex=1.5)
title(main="plot() with user defined colours")

##############################
# Plotting South-East Coastline and Islands in Moreton Bay
# Reference:  Coastline Extractor:  http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html
require(MASS)
MB.cl <- read.table("MB_coastline.txt")
names(MB.cl) <- c("Longitude","Latitude")
eqscplot(MB.cl$Long,MB.cl$Lat,type="l",xlab="Longitude",ylab="Latitude",
    ylim=c(range(MB.cl[,2],-27.33,na.rm=T)),xlim=c(range(MB.cl[,1],153.42,na.rm=T)))
# plotting island names and locations
nms <- c("Coochiemudlo Island","Green Island","Mud Island","Myora Point",
  "Peel Island","Polka Point","Empire Point","Wellington Point",
  "St Helena Island")
loc.lat <- c(-27.57,-27.425,-27.34,-27.47,-27.5,-27.5,-27.5,-27.47,-27.39)
loc.long <- c(153.33,153.235,153.26,153.41,153.35,153.40,153.265,153.24,153.235)
points(loc.long,loc.lat,pch=16)
text(loc.long,loc.lat,nms,pch=16,col="blue",pos=3)
text(153.32,-27.4,"Moreton Bay",cex=1.5,col="red")
text(153.25,-27.57,"Mainland",cex=1.5,col="red")
text(153.4,-27.55,"Nth Stradbroke",cex=1,col="red")
text(153.4,-27.56,"Island",cex=1,col="red")
points(153.29,-27.51,cex=1.2,col="blue",pch=17)
text(153.29,-27.51,"Cleveland Pt",cex=1.2,col="red",pos=3)

