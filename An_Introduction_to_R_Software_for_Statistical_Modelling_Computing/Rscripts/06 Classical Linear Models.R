######################## © CSIRO Australia 2005 ###############################
# Session 6: Classical Linear Models                                          #
# Authors:   Petra Kuhnert & Bill Venables                                    #
#            CSIRO Mathematical and Information Sciences                      #
# Date:      28 November 2005                                                 #
###############################################################################

#####################
# Linear Modelling
#####################
#####################
# Janka Hardness Data
# Plotting
janka <- read.csv("janka.csv")
with(janka,plot(Density,Hardness,col="blue"))
# Initial Model Building
# Linear or quadratic model
jank.1 <- lm(Hardness ~ Density, janka)
jank.2 <- update(jank.1, . ~ . + I(Density^2))
summary(jank.2)$coef
# A cubic model
jank.3 <- update(jank.2, . ~ . + I(Density^3))
summary(jank.3)$coef

# Stability of coefficients
janka <- transform(janka,d=Density-mean(Density))
jank.1 <- lm(Hardness ~ d, janka)
jank.2 <- update(jank.1, .~.+I(d^2))
jank.3 <- update(jank.2, .~.+I(d^3))

# Checks and Balances
trellis.par.set(col.whitebg())
require(MASS)
xyplot(studres(jank.2) ~ fitted(jank.2),
	aspect = 0.6,
  	panel = function(x, y, ...) {
      panel.xyplot(x, y, col = "navy", ...)
      panel.abline(h = 0, lty = 4, col = "red")
  }, xlab = "Fitted values", ylab = "Residuals")

qqmath(~ studres(jank.2), panel =
	function(x, y, ...) {
  		panel.qqmath(x, y, col = "navy", ...)
  		panel.qqmathline(y, qnorm, col = "red", lty=4)
  }, xlab = "Normal scores", aspect = 0.6,
        ylab = "Sorted studentized residuals")

# Transformations
boxcox(jank.2,lambda = seq(-0.25, 1, len=20))
# log transformation
ljank.2 <- update(jank.2, log(.)~.)
round(summary(ljank.2)$coef, 4)
lrs <- studres(ljank.2)
lfv <- fitted(ljank.2)
xyplot(lrs ~ lfv, panel =
    function(x, y, ...) {
      panel.xyplot(x, y, pch=16, ...)
      panel.abline(h=0, lty=4)
    },
    xlab = "Fitted (log trans.)",ylab = "Residuals (log trans.)", col = "red")
# Plot of transformed data
with(janka,plot(Density, Hardness, log = "y"))

#######################
# Iowa Wheat Data
#
# Multiple Regression
require(RODBC)
iowheat <-sqlFetch(odbcConnectExcel("Iowa.xls"),"Iowa")
names(iowheat)
bigm <- lm(Yield ~ ., data = iowheat)
# Drop terms individually
dropterm(bigm, test = "F")
# Fit small model and add terms sequentially
smallm <- update(bigm, . ~ Year)
addterm(smallm, bigm, test = "F")
# Automatic selection of variables
stepm <- stepAIC(bigm,scope = list(lower = ~ Year))
# Flexible version of linear regression
require(splines)
iowa.spline <- aov(Yield ~ ns(Rain0, 3) + 	ns(Rain2, 3) + ns(Temp4, 3) +
   ns(Year, 3),iowheat)
par(mfrow=c(2,2))
termplot(iowa.spline, se = TRUE, rug = TRUE,partial=TRUE)
# BV's function
tplot(iowa.spline, se = TRUE, rug = TRUE,partial=TRUE)


##########################
# Finxed & Random Effects Model: Petroleum Data
require(MASS)  # only if the MASS library has not already been attached
names(petrol)
xyplot(Y ~ EP | No, petrol, as.table = T,
    panel = function(x, y, ...) {
    	panel.xyplot(x, y, ...)
    	panel.lmline(x, y, ...)
    }, xlab = "End point", ylab = "Yield (%)",
    main = "Petroleum data of N L Prater")

# Fixed Effect Models
pet.2 <- aov(Y ~ No*EP, petrol)
pet.1 <- update(pet.2, .~.-No:EP)
pet.0 <- update(pet.1, .~.-No)
anova(pet.0, pet.1, pet.2)

# Random Effect Models
library(nlme)
pet.re1 <- lme(Y ~ EP, petrol, random = ~1+EP|No)
summary(pet.re1)

# Shrinkage Effect
B <- coef(pet.re1)
xyplot(Y ~ EP | No, petrol, as.table = T, subscripts = T,
    panel = function(x, y, subscripts, ...) {
    	panel.xyplot(x, y, ...)
    	panel.lmline(x, y, col="navy",...)
    	wh <- as(petrol$No[subscripts][1], "character")
    	browser()
    	panel.abline(B[wh, 1], B[wh, 2],  col = "green")
    }, xlab = "End point", ylab = "Yield (%)")






