## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/arsenic

library("arm")
wells <- read.table ("wells.dat")
attach.all (wells)

## Histogram on distance (Figure 5.8)

hist (dist, breaks=seq(0,10+max(dist[!is.na(dist)]),10), 
   xlab="Distance (in meters) to the nearest safe well", 
   ylab="", main="", mgp=c(2,.5,0))

## Logistic regression with one predictor

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.1)

## Repeat the regression above with distance in 100-meter units

dist100 <- dist/100
fit.2 <- glm (switch ~ dist100, family=binomial(link="logit"))
display (fit.2)

## Graphing the fitted model with one predictor (Figure 5.9)

jitter.binary <- function(a, jitt=.05){
  ifelse (a==0, runif (length(a), 0, jitt), runif (length(a), 1-jitt, 1))
}

switch.jitter <- jitter.binary(switch)

plot(dist, switch.jitter, xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x), lwd=1, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)

## Histogram on arsenic levels (Figure 5.10)

hist (arsenic, breaks=seq(0,.25+max(arsenic[!is.na(arsenic)]),.25), freq=TRUE, xlab="Arsenic concentration in well water", ylab="", main="", mgp=c(2,.5,0))

## Logistic regression with second input variable

fit.3 <- glm (switch ~ dist100 + arsenic, family=binomial(link="logit"))
display (fit.3)

## Graphing the fitted model with two predictors (Figure 5.11)

plot(dist, switch.jitter, xlim=c(0,max(dist)), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(cbind (1, x/100, .5) %*% coef(fit.3)), lwd=.5, add=TRUE)
curve (invlogit(cbind (1, x/100, 1.0) %*% coef(fit.3)), lwd=.5, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
text (50, .27, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)

plot(arsenic, switch.jitter, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(cbind (1, 0, x) %*% coef(fit.3)), lwd=.5, add=TRUE)
curve (invlogit(cbind (1, 0.5, x) %*% coef(fit.3)), lwd=.5, add=TRUE)
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
text (1.5, .78, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)

 #equivalently

plot(dist, switch.jitter, xlim=c(0,max(dist)), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*.50), lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*1.00), lwd=.5, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
text (50, .27, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)

plot(arsenic, switch.jitter, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0.5+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
text (1.5, .78, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)



