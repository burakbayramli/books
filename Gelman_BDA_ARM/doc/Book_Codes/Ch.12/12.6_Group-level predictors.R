## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.3_Partial pooling with predictors.R") # where variables were defined
# close the Bugs window to proceed

## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

## Varying-intercept model w/ group-level predictors
u.full <- u[county]
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)

coef (M2)
fixef (M2)
ranef (M2)

## Plots on Figure 12.5
M1 <- lmer (y ~ x + (1 | county))
a.hat.M1 <- fixef(M1)[1] + ranef(M1)$county                
b.hat.M1 <- fixef(M1)[2]

a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + ranef(M2)$county
b.hat.M2 <- fixef(M2)[2]

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.1, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (a.hat.M1[j,] + b.hat.M1*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M2[j,] + b.hat.M2*x, lwd=1, col="black", add=TRUE)
}

# Plot of ests & se's vs. county uranium (Figure 12.6)
a.se.M2 <- se.coef(M2)$county

par (mar=c(5,5,4,2)+.1)
plot (u, t(a.hat.M2), cex.lab=1.2, cex.axis=1.1,
      xlab="county-level uranium measure", ylab="est. regression intercept", pch=20,
      ylim=c(0.5,2.0), yaxt="n", xaxt="n", mgp=c(3.5,1.2,0))
axis (1, seq(-1,1,.5), cex.axis=1.1, mgp=c(3.5,1.2,0))
axis (2, cex.axis=1.1, mgp=c(3.5,1.2,0))
curve (fixef(M2)["(Intercept)"] + fixef(M2)["u.full"]*x, lwd=1, col="black", add=TRUE)
for (j in 1:J){
  lines (rep(u[j],2), a.hat.M2[j,] + c(-1,1)*a.se.M2[j,], lwd=.5, col="gray10")
}


