# Radon computations for chapter 13 of the book

# 1.  varying-intercept, varying-slope with no group-level predictors

# fit the model

M3 <- lmer (y ~ x + (1 + x | county))
display (M3)
coef (M3)

# make the graphs

a.hat.M3 <- coef(M4)$unmodeled[1] + coef(M1)$county[,1]
b.hat.M3 <- coef(M3)$unmodeled[2] + coef(M1)$county[,2]

b.hat.unpooled.varying <- array (NA, c(J,2))
for (j in 1:J){
  lm.unpooled.varying <- lm (y ~ x, subset=(county==j))
  b.hat.unpooled.varying[j,] <- coef(lm.unpooled.varying)
}

postscript ("c:/books/multilevel/counties.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (b.hat.unpooled.varying[j,1] + b.hat.unpooled.varying[j,2]*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M3[j] + b.hat.M3[j]*x, lwd=1, col="black", add=TRUE)
}
dev.off()

# 2.  varying-intercept, varying-slope with uranium as a group-level predictor

# fit model using lmer

M4 <- lmer (y ~ x + u.full + x:u.full + (1 + x | county))
display (M4)
coef (M4)
a.hat.M4 <- coef(M4)$unmodeled[1] + coef(M4)$unmodeled[3]*u + coef(M4)$county[,1]
b.hat.M4 <- coef(M4)$unmodeled[2] + coef(M4)$unmodeled[4]*u + coef(M4)$county[,2]
a.se.M4 <- se.coef(M4)$county
b.se.M4 <- se.coef(M4)$county

# plot estimated intercepts and slopes

lower <- a.hat.M4 - a.se.M4
upper <- a.hat.M4 + a.se.M4
postscript ("c:/books/multilevel/intercepts.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (u, a.hat.M4, cex.lab=2.4, cex.axis=2.2, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression intercept", pch=20)
curve (coef(M4)$unmodeled["(Intercept)"] + coef(M4)$unmodeled["u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
dev.off()

lower <- b.hat.M4 - b.se.M4
upper <- b.hat.M4 + b.se.M4
postscript ("c:/books/multilevel/slopes.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (u, b.hat.M4, cex.lab=2.4, cex.axis=2.2, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression slope", pch=20)
curve (coef(M4)$unmodeled["x"] + coef(M4)$unmodeled["x:u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray10")
dev.off()
