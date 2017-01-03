## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.6_Group-level predictors.R") # where variables were defined
# close the Bugs window to proceed

## Varying intercept & slopes w/ no group level predictors
M3 <- lmer (y ~ x + (1 + x | county))
display (M3)

coef (M3)
fixef (M3)
ranef (M3)

 # plots on Figure 13.1
a.hat.M3 <- fixef(M3)[1] + ranef(M3)$county[,1] 
b.hat.M3 <- fixef(M3)[2] + ranef(M3)$county[,2]

b.hat.unpooled.varying <- array (NA, c(J,2))
for (j in 1:J){
  lm.unpooled.varying <- lm (y ~ x, subset=(county==j))
  b.hat.unpooled.varying[j,] <- coef(lm.unpooled.varying)
}

lm.pooled <- lm (y ~ x)

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.1, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (b.hat.unpooled.varying[j,1] + b.hat.unpooled.varying[j,2]*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M3[j] + b.hat.M3[j]*x, lwd=1, col="black", add=TRUE)
}

## Including group level predictors
M4 <- lmer (y ~ x + u.full + x:u.full + (1 + x | county))
display (M4)

coef (M4)
fixef (M4)
ranef (M4)

a.hat.M4 <- fixef(M4)[1] + fixef(M4)[3]*u + ranef(M4)$county[,1]
b.hat.M4 <- fixef(M4)[2] + fixef(M4)[4]*u + ranef(M4)$county[,2]
a.se.M4 <- se.ranef(M4)$county[,1]
b.se.M4 <- se.ranef(M4)$county[,2]

 # plot on Figure 13.2(a)
lower <- a.hat.M4 - a.se.M4
upper <- a.hat.M4 + a.se.M4
par (mar=c(5,5,4,2)+.1)
plot (u, a.hat.M4, cex.lab=1.2, cex.axis=1.1, ylim=range(lower,upper), 
      xlab="county-level uranium measure", ylab="regression intercept", 
      pch=20, yaxt="n")
axis (2, c(0,1,1.5,2))
curve (fixef(M4)[1] + fixef(M4)[3]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
mtext ("Intercepts", line=1)

 # plot on Figure 13.2(b)
lower <- b.hat.M4 - b.se.M4
upper <- b.hat.M4 + b.se.M4
par (mar=c(5,5,4,2)+.1)
plot (u, b.hat.M4, cex.lab=1.2, cex.axis=1.1, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression slope", pch=20)
curve (fixef(M4)[2] + fixef(M4)[4]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
mtext ("Slopes", line=1)
