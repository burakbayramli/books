## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.3_Partial pooling with predictors.R") # where variables were defined
# close the Bugs window to proceed

## Varying-intercept model w/ no predictors
M0 <- lmer (y ~ 1 + (1 | county))
display (M0)

## Including x as a predictor
M1 <- lmer (y ~ x + (1 | county))
display (M1)

  # estimated regression coefficicents
coef (M1)

  # fixed and random effects
fixef (M1)
ranef (M1)

  # uncertainties in the estimated coefficients
se.fixef (M1)
se.ranef (M1)

  # 95% CI for the slope
fixef(M1)["x"] + c(-2,2)*se.fixef(M1)["x"]
#or
fixef(M1)[2] + c(-2,2)*se.fixef(M1)[2]

  # 95% CI for the intercept in county 26
coef(M1)$county[26,1] + c(-2,2)*se.ranef(M1)$county[26]

  # 95% CI for the error in the intercept in county 26
as.matrix(ranef(M1)$county)[26] + c(-2,2)*se.ranef(M1)$county[26]

  # to plot Figure 12.4
a.hat.M1 <- coef(M1)$county[,1]                # 1st column is the intercept
b.hat.M1 <- coef(M1)$county[,2]                # 2nd element is the slope

par (mfrow=c(2,4))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", main=uniq[j],cex.lab=1.2,
    cex.axis=1.1, pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.1)
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1)
  axis (2, c(-1,1,3), mgp=c(2,.7,0), cex.axis=1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lty=2, col="gray10", add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, col="gray10", add=TRUE)
  curve (a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, col="black", add=TRUE)
}  

## Multilevel model ests vs. sample size (plot on the right on figure 12.3)
a.se.M1 <- se.coef(M1)$county

par (mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, t(a.hat.M1), cex.lab=1.2, cex.axis=1.1,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (multilevel model)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=1.1)
axis (2, seq(0,3), cex.axis=1.1)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    as.vector(a.hat.M1[j]) + c(-1,1)*a.se.M1[j], lwd=.5, col="gray10")
}
abline (coef(lm.pooled)[1], 0, lwd=.5)















