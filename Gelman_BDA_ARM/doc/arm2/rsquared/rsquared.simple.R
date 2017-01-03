# simple graphs showing r-squared

heightweight <- read.fwf ("wfw90.dat", widths=c(143,1,2,3))
height1 <- heightweight[,2]
height2 <- heightweight[,3]
weight <- heightweight[,4]
ok <- !is.na (height1+height2+weight) & weight<998 & height1<7 & height2<12
height <- 12*height1[ok] + height2[ok]
weight <- weight[ok]
n <- length(height)

# display data

lm.0 <- lm (log(weight) ~ height)
noise <- rnorm (n, 0, 1.8*sigma.hat(lm.0))
lm.1 <- lm (noise ~ height)
noise <- noise - beta.hat(lm.1)[1] - beta.hat(lm.1)[2]*height

height.jitter <- height + runif(n,-.15,.15)
x.range <- range(height.jitter)

postscript ("c:/books/multilevel/rsq1a.ps", height=2.7, width=3.3)
par (mar=c(4,3,1,1))
plot (height.jitter, log(weight), pch=20, xlab="height", ylab="log (weight)",
      mgp=c(1.5,.5,0), ylim=range(log(weight)+noise), xlim=x.range, xaxt="n", yaxt="n", main="", cex=.4)
abline (beta.hat(lm.0)[1], beta.hat(lm.0)[2], lwd=.5)
axis (1, seq(60,80,10), mgp=c(1.8,.5,0))
axis (2, seq(3,6), mgp=c(1.8,.5,0))
print (sigma.hat(lm.0))
text (79, 4.3, expression(paste(hat(sigma)," = ",0.17)))
dev.off()

# add noise

postscript ("c:/books/multilevel/rsq1b.ps", height=2.7, width=3.3)
par (mar=c(4,3,1,1))
plot (height.jitter, log(weight)+noise, pch=20, xlab="height", ylab="log (weight)",
      mgp=c(1.5,.5,0), ylim=range(log(weight)+noise), xlim=x.range, xaxt="n", yaxt="n", main="", cex=.4)
abline (beta.hat(lm.0)[1], beta.hat(lm.0)[2], lwd=.5)
axis (1, seq(60,80,10), mgp=c(1.8,.5,0))
axis (2, seq(3,6), mgp=c(1.8,.5,0))
print (sigma.hat(lm.1))
text (79, 4.3, expression(paste(hat(sigma)," = ",0.32)))
dev.off()

# restrict range

postscript ("c:/books/multilevel/rsq2a.ps", height=2.7, width=3.3)
par (mar=c(4,3,1,1))
plot (height.jitter, log(weight), pch=20, xlab="height", ylab="log (weight)",
      mgp=c(1.5,.5,0), ylim=range(log(weight)+noise), xlim=x.range, xaxt="n", yaxt="n", main="", cex=.4)
abline (beta.hat(lm.0)[1], beta.hat(lm.0)[2], lwd=.5)
axis (1, seq(60,80,10), mgp=c(1.8,.5,0))
axis (2, seq(3,6), mgp=c(1.8,.5,0))
text (78, 6, expression(paste(R^2," = 30%")))
dev.off()

keep <- height>=65 & height<=70
lm.2 <- lm (log(weight) ~ height, subset=keep)
display (lm.2)

postscript ("c:/books/multilevel/rsq2b.ps", height=2.7, width=3.3)
par (mar=c(4,3,1,1))
plot ((height.jitter)[keep], log(weight)[keep], pch=20, xlab="height", ylab="log (weight)",
      mgp=c(1.5,.5,0), ylim=range(log(weight)+noise), xlim=x.range, xaxt="n", yaxt="n", main="", cex=.4)
abline (beta.hat(lm.0)[1], beta.hat(lm.0)[2], lwd=.5)
axis (1, seq(60,80,10), mgp=c(1.8,.5,0))
axis (2, seq(3,6), mgp=c(1.8,.5,0))
text (78, 6, expression(paste(R^2," = 10%")))
dev.off()
