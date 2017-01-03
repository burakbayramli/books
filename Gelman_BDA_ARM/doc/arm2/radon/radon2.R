# Varying-intercept, varying-slope models

library (arm)
source ("radon_setup.R")

# 1.  varying-intercept, varying-slope with no group-level predictors

# fit the model

M3 <- lmer (y ~ x + (1 + x | county))
display (M3)

# make the graphs

a.hat.M3 <- fixef(M3)[1] + ranef(M3)$county[,1]
b.hat.M3 <- fixef(M3)[2] + ranef(M3)$county[,2]

b.hat.unpooled.varying <- array (NA, c(J,2))
for (j in 1:J){
  lm.unpooled.varying <- lm (y ~ x, subset=(county==j))
  b.hat.unpooled.varying[j,] <- coef(lm.unpooled.varying)
}

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="red", add=TRUE)
  curve (b.hat.unpooled.varying[j,1] + b.hat.unpooled.varying[j,2]*x, lwd=.5, col="blue", add=TRUE)
  curve (a.hat.M3[j] + b.hat.M3[j]*x, lwd=1, col="black", add=TRUE)
}

# 2.  varying-intercept, varying-slope with uranium as a group-level predictor

# fit model using lmer

M4 <- lmer (y ~ x + u.full + x:u.full + (1 + x | county))
display (M4)
a.hat.M4 <- fixef(M4)["(Intercept)"] + fixef(M4)["u.full"]*u + ranef(M4)$county[,"(Intercept)"]
b.hat.M4 <- fixef(M4)["x"] + fixef(M4)["x:u.full"]*u + ranef(M4)$county[,"x"]
a.se.M4 <- se.coef(M4)$county[,"(Intercept)"]  # for simplicity, ignoring s.e. of the "fixed effects" component
b.se.M4 <- se.coef(M4)$county[,"x"]  # for simplicity, ignoring s.e. of the "fixed effects" component


# plot estimated intercepts and slopes

par (mfrow=c(2,2))
par (mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)

lower <- a.hat.M4 - a.se.M4
upper <- a.hat.M4 + a.se.M4
plot (u, a.hat.M4, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression intercept", pch=20)
curve (fixef(M4)["(Intercept)"] + fixef(M4)["u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
mtext ("Intercepts", line=1)

lower <- b.hat.M4 - b.se.M4
upper <- b.hat.M4 + b.se.M4
plot (u, b.hat.M4, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression slope", pch=20)
curve (fixef(M4)["x"] + fixef(M4)["x:u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
mtext ("Slopes", line=1)
