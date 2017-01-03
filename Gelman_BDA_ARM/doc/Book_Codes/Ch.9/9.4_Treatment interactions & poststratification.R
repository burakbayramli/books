## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

# The R codes & data files should be saved in the same directory for
# the source command to work

source("9.3_Randomized experiments.R") # where data was cleaned

## Treatment interactions and poststratification

 # model with only treat. indicator

lm.1 <- lm (post.test ~ treatment, subset=(grade==4))
display (lm.1) 

 # model controlling for pre-test

lm.2 <- lm (post.test ~ treatment + pre.test, subset=(grade==4))
display (lm.2) 

## More than 2 treat. levels, continuous treat., multiple treat. factors (Figure 9.7)

par (mfrow=c(1,4), pty="s")
for (j in 1:4){
  ok <- Grade==j
  x <- c (treated.Pretest[ok], control.Pretest[ok])
  y <- c (treated.Posttest[ok], control.Posttest[ok])
  t <- rep (c(1,0), rep(sum(ok),2))

#  plot (x,y, type="n", main=paste("grade",j),
    plot (c(0,125),c(0,125), type="n", main=paste("grade",j),
        xlab=expression(paste("pre-test, ",x[i])),
        ylab=expression(paste("post-test, ",y[i])),
        cex.axis=1.0, cex.lab=1.1, cex.main=1.2, mgp=c(2.5,.7,0))
  lm.1 <- lm (y ~ x + t + x*t)
  abline (lm.1$coef[1], lm.1$coef[2], lwd=.5, lty=2)
  abline (lm.1$coef[1] + lm.1$coef[3], lm.1$coef[2] + lm.1$coef[4], lwd=.5)
  points (control.Pretest[ok], control.Posttest[ok], pch=20, cex=1.1)
  points (treated.Pretest[ok], treated.Posttest[ok], pch=21, cex=1.1)
}

## model to display uncertainty & Figure 9.8

lm.4 <- lm (post.test ~ treatment + pre.test + treatment:pre.test, subset=(grade==4))
display (lm.4)

par (mfrow=c(1,1), mar=c(5,4,3,3))
lm.4.sim <- sim (lm.4)
plot (0, 0, xlim=range (pre.test[grade==4]), ylim=c(-5,10),
       xlab="pre-test", ylab="treatment effect", main="treatment effect in grade 4")
abline (0, 0, lwd=.5, lty=2)
for (i in 1:20){
  curve (lm.4.sim$coef[i,2] + lm.4.sim$coef[i,4]*x, lwd=.5, col="gray", add=TRUE)}
curve (coef(lm.4)[2] + coef(lm.4)[4]*x, lwd=.5, add=TRUE)

 # compute the average treatment effect & summarize

n.sims <- nrow(lm.4.sim$coef)
effect <- array (NA, c(n.sims, sum(grade==4)))
for (i in 1:n.sims){
  effect[i,] <- lm.4.sim$coef[i,2] + lm.4.sim$coef[i,4]*pre.test[grade==4]
}
avg.effect <- rowMeans (effect)

print (c (mean(avg.effect), sd(avg.effect)))
