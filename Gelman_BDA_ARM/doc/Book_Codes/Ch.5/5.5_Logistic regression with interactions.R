## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/arsenic

# The R codes & data files should be saved in the same directory for
# the source command to work

source("5.4_Logistic regression_wells in Bangladesh.R") # where variables were defined

# Logistic regression with interactions

fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic, 
  family=binomial(link="logit"))
display(fit.4)

## Centering the input variables

c.dist100 <- dist100 - mean (dist100)
c.arsenic <- arsenic - mean (arsenic)

## Refitting the model with centered inputs

fit.5 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,
  family=binomial(link="logit"))
display(fit.5)

## Graphing the model with interactions (Figure 5.12)

plot(dist, switch.jitter, xlim=c(0,max(dist)), xlab="Distance (in meters) to nearest safe well", 
   ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(cbind (1, x/100, .5, .5*x/100) %*% coef(fit.4)), lwd=.5, add=TRUE)
curve (invlogit(cbind (1, x/100, 1.0, 1.0*x/100) %*% coef(fit.4)), lwd=.5, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
text (50, .37, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)

plot(arsenic, switch.jitter, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water",
   ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(cbind (1, 0, x, 0*x) %*% coef(fit.4)), lwd=.5, add=TRUE)
curve (invlogit(cbind (1, 0.5, x, 0.5*x) %*% coef(fit.4)), lwd=.5, add=TRUE)
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
text (1.5, .8, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)

 #equivalently

plot(dist, switch.jitter, xlim=c(0,max(dist)), xlab="Distance (in meters) to nearest safe well", 
   ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*.50+coef(fit.4)[4]*(x/100)*.50),
   lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*1.00+coef(fit.4)[4]*(x/100)*1),
   lwd=.5, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
text (50, .37, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)

plot(arsenic, switch.jitter, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water",
   ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0+coef(fit.4)[3]*x+coef(fit.4)[4]*0*x), from=0.5,
   lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0.5+coef(fit.4)[3]*x+coef(fit.4)[4]*0.5*x), 
  from=0.5, lwd=.5, add=TRUE)
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
text (1.5, .8, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)

## Adding social predictors

educ4 <- educ/4

 # with community organization variable

fit.6 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  assoc + educ4, family=binomial(link="logit"))
display (fit.6)

 # without community organization variable

fit.7 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  educ4, family=binomial(link="logit"))
display (fit.7)

## Adding further interactions (centering education variable)

c.educ4 <- educ4 - mean(educ4)

fit.8 <- glm (switch ~ c.dist100 + c.arsenic + c.educ4 + c.dist100:c.arsenic +
  c.dist100:c.educ4 + c.arsenic:c.educ4, family=binomial(link="logit"))
display (fit.8)







