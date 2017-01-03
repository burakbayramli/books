## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.6_Group-level predictors.R") # where variables were defined
# close the Bugs window to proceed

## Complete pooling regression
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## No pooling regression
lm.unpooled <- lm (y ~ x + factor(county)-1)
display (lm.unpooled)

## Multilevel model with no group-level predictors
M1 <- lmer (y ~ x + (1 | county))

display <- c (36, 35, 21, 61)  # counties to display

## Plot Figure 18.5
coef.unpooled <- coef(lm.unpooled)
se.unpooled <- se.coef(lm.unpooled)

mu.a <- fixef(M1)[1]
sigma.a <- 0.33
a.j <- rep(NA,J)
a.j <- fixef(M1)[1] + ranef(M1)$county
a.j.sigma <- rep(NA,J)
a.j.sigma <- se.coef(M1)$county

par(mfrow=c(3,4))
for (j in display){
  curve(dnorm(x, coef.unpooled[j+1], se.unpooled[j+1]), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste(uniq[j],":likelihood"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}

for (j in display){
  curve(dnorm(x, mu.a, sigma.a), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste("prior dist"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}

for (j in display){
  curve(dnorm(x, a.j[j,], a.j.sigma[j]), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste("posterior dist"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}

## Multilevel model with group-level predictors
M2 <- lmer (y ~ x + u.full + (1 | county))

## Plot Figure 18.6
coef.unpooled <- coef(lm.unpooled)
se.unpooled <- se.coef(lm.unpooled)

sigma.a <- 0.16
a.j <- rep(NA,J)
a.j <- fixef(M2)[1] + ranef(M2)$county
a.j.sigma <- rep(NA,J)
a.j.sigma <- se.coef(M2)$county

par(mfrow=c(3,4))
for (j in display){
  curve(dnorm(x, coef.unpooled[j+1], se.unpooled[j+1]), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste(uniq[j],":likelihood"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}

for (j in display){
  curve(dnorm(x, fixef(M2)[1] + (fixef(M2)[3])*u[j], sigma.a), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste("prior dist"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}

for (j in display){
  curve(dnorm(x, a.j[j,] + (fixef(M2)[3])*u[j], a.j.sigma[j]), from=-0.5, to=4.5,
  xlab=expression(alpha[j]), ylab="", main=paste("posterior dist"),
  yaxt="n", bty="n", cex.lab=1.2, cex.axis=1.1, cex.main=0.8)
}