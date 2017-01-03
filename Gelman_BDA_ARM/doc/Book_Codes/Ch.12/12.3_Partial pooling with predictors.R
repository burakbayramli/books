## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.2_Partial pooling with no predictors.R") # where data was cleaned
# close the Bugs window to proceed

## Complete pooling regression
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## No pooling regression
lm.unpooled <- lm (y ~ x + factor(county) -1)
display (lm.unpooled)

## Comparing-complete pooling & no-pooling (Figure 12.2)
x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)  # counties to be displayed
y.range <- range (y[!is.na(match(county,display8))])

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1,
        main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, add=TRUE)
}

## No-pooling ests vs. sample size (plot on the left on figure 12.3)
sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))

par (mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, coef(lm.unpooled)[-1], cex.lab=1.2, cex.axis=1.2,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (no pooling)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=1.1)
axis (2, seq(0,3), cex.axis=1.1)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    coef(lm.unpooled)[j+1] + c(-1,1)*se.coef(lm.unpooled)[j+1], lwd=.5)
}

