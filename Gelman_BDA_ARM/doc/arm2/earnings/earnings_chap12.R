# height/earnings regressions for chapter 12

# 1.  Varying-intercept, varying-slope model of earnings on height, with ethnicity categories

# set up the data

attach.all (heights.clean0)

y <- log(earn)
x <- height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- age.category

# regression of log (earnings) on height, age, and ethnicity

M1 <- lmer (y ~ x + (1 + x | eth))
display (M1)
ab.hat.M1 <- beta.hat(M1)$eth
a.hat.M1 <- ab.hat.M1[,1]
b.hat.M1 <- ab.hat.M1[,2]

ab.sim.M1 <- sim(M1)$eth
a.sim.M1 <- ab.sim.M1[,,1]
b.sim.M1 <- ab.sim.M1[,,2]

# display as graph

x.jitter <- x + runif(n, -.2,.2)
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

postscript ("c:/books/multilevel/heights.mult1a.ps", height=2.5, horizontal=T)
par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot (x.jitter[eth==j], y[eth==j], xlab="height (inches)", ylab="log earnings", mgp=c(2,.7,0), xlim=range(x), ylim=range(y),
        yaxt="n",
        cex.lab=1.4, cex.axis=1.3, pch=20, cex=.6, cex.main=1.5, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.3, mgp=c(2,.5,0))
  for (i in 1:20){
    curve (a.sim.M1[i,j] + b.sim.M1[i,j]*x, lwd=.5, col="gray", add=TRUE)
  }
  curve (a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, add=TRUE)
}
dev.off()

postscript ("c:/books/multilevel/heights.mult2a.ps", horizontal=T)
par (mar=c(7,8,4,2)+.1)
plot (a.hat.M1, b.hat.M1,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=3, cex.lab=3.3, cex=3, mgp=c(5.5,1.5,0), type="n", yaxt="n")
axis (2, seq(0.03,0.06,0.01), cex.axis=3, mgp=c(4.5, 1.5,0))
text (a.hat.M1, b.hat.M1, substr (eth.label,1,1), cex=3.3)
dev.off()


postscript ("c:/books/multilevel/heights.mult3a.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (x.jitter, y, xlim=c(0,79), xlab="height (inches)", ylab="log earnings",
      cex.lab=2.5, cex.axis=2.5, xaxs="i", pch=20, cex=.8, yaxt="n")
axis (2, seq(6,12,2), cex.axis=2.5)
for (j in 1:4){
  curve (a.hat.M1[j] + b.hat.M1[j]*x, add=TRUE)
}
adj1 <- c(-3, 2, -3, 0)
adj2 <- c(.3, -.4, .4, +.4)
adj1 <- c(-3,0,0,3)
adj2 <- c(.2,0,0,-.2)
text (20 + adj1, a.hat.M1 + b.hat.M1*20 + adj2, eth.label, cex=2.5)
dev.off()

# center height by subtracting its mean

x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)
M2 <- lmer (y ~ x.centered + (1 + x.centered | eth))
display (M2)

ab.hat.M2 <- beta.hat(M2)$eth
a.hat.M2 <- ab.hat.M2[,1]
b.hat.M2 <- ab.hat.M2[,2]

ab.sim.M2 <- sim(M2)$eth
a.sim.M2 <- ab.sim.M2[,,1]
b.sim.M2 <- ab.sim.M2[,,2]

# display as graph


postscript ("c:/books/multilevel/heights.mult4a.ps", height=2.5, horizontal=T)
par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot (x.centered.jitter[eth==j], y[eth==j], xlab="height (inches from mean)", ylab="log earnings", mgp=c(2,.7,0), xlim=range(x.centered), ylim=range(y),
        yaxt="n",
        cex.lab=1.4, cex.axis=1.3, pch=20, cex=.6, cex.main=1.5, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.3, mgp=c(2,.5,0))
  for (i in 1:20){
    curve (a.sim.M2[i,j] + b.sim.M2[i,j]*x, lwd=.5, col="gray", add=TRUE)
  }
  curve (a.hat.M2[j] + b.hat.M2[j]*x, lwd=1, add=TRUE)
}
dev.off()

postscript ("c:/books/multilevel/heights.mult5a.ps", horizontal=T)
par (mar=c(7,8,4,2)+.1)
plot (a.hat.M2, b.hat.M2,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=3, cex.lab=3.3, cex=3, mgp=c(5.5,1.5,0), type="n", yaxt="n")
axis (2, seq(0.03,0.06,0.01), cex.axis=3, mgp=c(4.5, 1.5,0))
text (a.hat.M2, b.hat.M2, substr (eth.label,1,1), cex=3.3)
dev.off()




postscript ("c:/books/multilevel/heights.mult6.ps", height=7.5, horizontal=T)
par (mfrow=c(3,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:3){
  for (k in 1:4){
     plot (x.jitter[age==j&eth==k], y[age==j&eth==k], xlab="height (inches from mean)", ylab="log earnings",
        cex.lab=1.2, cex.axis=1.2, pch=20, cex=.6, cex.main=1.5, xlim=range(x),
           ylim=range(y), mgp=c(2,.7,0), yaxt="n",
        main=paste(eth.label[k], ", ", age.label[j], sep=""))
     axis (2, seq(6,12,2))
     a.00 <- height.2$median$b[1,j,k]
     b.00 <- height.2$median$b[2,j,k]
     for (i in 21:40)
       curve (b[i,1,j,k] + b[i,2,j,k]*x, lwd=.5, col="gray", add=T)
     curve (a.00 + b.00*x, lwd=2, add=T)
  }
}
dev.off()

