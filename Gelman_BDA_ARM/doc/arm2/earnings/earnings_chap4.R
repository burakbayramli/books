# classical regressions and graphs for earnings example in chapter 4

attach.all (heights.clean)
height.jitter.add <- runif (n, -.2, .2)

# model on original scale

lm.earn0 <- lm (earn ~ height)
display (lm.earn0)
sim.earn0 <- sim (lm.earn0)

postscript ("c:/books/multilevel/heights.simple0a.ps", horizontal=T)
par (mar=c(6,6,4,2)+.1)
plot (height + height.jitter.add, earn, xlab="height", ylab="earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="Fitted linear model")
axis (2, c(0,100000,200000), c("0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
for (i in 1:20){
  curve (sim.earn0$beta[i,1] + sim.earn0$beta[i,2]*x, lwd=.5, col="gray30", add=TRUE)}
curve (beta.hat(lm.earn0)[1] + beta.hat(lm.earn0)[2]*x, add=TRUE)
dev.off()

postscript ("c:/books/multilevel/heights.intercept.ps", horizontal=T)
par (mar=c(6,6,4,2)+.1)
plot (height + height.jitter.add, earn, xlab="height", ylab="earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="x-axis extended to 0",
      xlim=c(0,max(height)), ylim=c(-110000,200000))
axis (2, c(-100000,0,100000,200000), c("-100000","0","100000",""), mgp=c(4,1.1,0),cex.axis=3)
for (i in 1:20){
  curve (sim.earn0$beta[i,1] + sim.earn0$beta[i,2]*x, lwd=.5, col="gray30", add=TRUE)}
curve (beta.hat(lm.earn0)[1] + beta.hat(lm.earn0)[2]*x, add=TRUE)
dev.off()

# model on log scale

log.earn <- log (earn)
log.height <- log (height)
earn.logmodel.1 <- lm (log.earn ~ height)
earn.loglogmodel.1 <- lm (log.earn ~ log.height)
earn.logmodel.2 <- lm (log.earn ~ height + male)
earn.loglogmodel.2 <- lm (log.earn ~ log.height + male)
earn.logmodel.3 <- lm (log.earn ~ height + male + height:male)
z.height <- (height - mean(height))/sd(height)
earn.logmodel.3a <- lm (log.earn ~ z.height + male + z.height:male)

display (earn.logmodel.1)
sim.logmodel.1 <- sim (earn.logmodel.1)

postscript ("c:/books/multilevel/heights.log1a.ps", horizontal=T)
par (mar=c(6,6,4,2)+.1)
plot (height + runif(n,-.2,.2), log.earn, xlab="height", ylab="log (earnings)", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="Log regression, plotted on log scale")
axis (2, seq(6,12,2), mgp=c(4,1.1,0),cex.axis=3)
for (i in 1:20)
  curve (sim.logmodel.1$beta[i,1] + sim.logmodel.1$beta[i,2]*x, lwd=.5, col="gray30", add=T)
curve (beta.hat(earn.logmodel.1)[1] + beta.hat(earn.logmodel.1)[2]*x, add=T)
dev.off()

postscript ("c:/books/multilevel/heights.log1b.ps", horizontal=T)
par (mar=c(6,6,4,2)+.1)
plot (height + runif(n,-.2,.2), earn, xlab="height", ylab="earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="Log regression, plotted on original scale")
axis (2, c(0,100000,200000), c("0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
for (i in 1:20)
  curve (exp(sim.logmodel.1$beta[i,1] + sim.logmodel.1$beta[i,2]*x), lwd=.5, col="gray30", add=T)
curve (exp(beta.hat(earn.logmodel.1)[1] + beta.hat(earn.logmodel.1)[2]*x), add=T)
dev.off()

