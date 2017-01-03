# Radon computations for chapter 12 of the book

# 1.  Computations for Figure 12.1

######    no predictors
ybarbar = mean(y)

cty.mns = tapply(y,county,mean)
cty.vars = tapply(y,county,var)
cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size)
cty.sds.sep = sqrt(tapply(y,county,var)/sample.size)

######## varying-intercept model, no predictors

radon.data <- list ("n", "J", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a")

mlm.radon.nopred <- bugs (radon.data, radon.inits, radon.parameters, "radon.multilevel.nopred.bug", n.chains=3, n.iter=2000)


###

sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))

postscript ("c:/books/multilevel/radon.nopred.ps", horizontal=T,height=3.5, width=7)
par(mfrow=c(1,2))
# pooled var ests
plot (sample.size.jittered, cty.mns, cex.lab=.9, cex.axis=1,
      xlab="sample size in county j",
      ylab="avg. log radon in county j",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,3.2), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         cty.mns[j] + c(-1,1)*cty.sds[j], lwd=.5)
#         cty.mns[j] + c(-1,1)*mean(cty.sds[!is.na(cty.sds)]), lwd=.5)
}
abline(h=mlm.radon.nopred$median$mu.a)
title("No pooling",cex.main=.9, line=1)
#abline(h=ybarbar)
points(sample.size.jittered[36],cty.mns[36],cex=4)#,col="red")
# mlm var ests

plot (sample.size.jittered, mlm.radon.nopred$median$a, cex.lab=.9, cex.axis=1,
      xlab="sample size in county j",
      ylab="avg. log radon in county j",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,3.2), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         mlm.radon.nopred$median$a[j] + c(-1,1)*mlm.radon.nopred$sd$a[j],
      lwd=.5)
}
abline(h=mlm.radon.nopred$median$mu.a)
points(sample.size.jittered[36],mlm.radon.nopred$median$a[36],cex=4)#,col="red")
title("Multilevel model",cex.main=.9, line=1)
dev.off()




# 2.  No-pooling and complete-pooling models

# fit the models

lm.pooled <- lm (y ~ x)
display (lm.pooled)

lm.unpooled.0 <- lm (y ~ x + factor(county))
display (lm.unpooled.0)

lm.unpooled <- lm (y ~ x + factor(county) - 1)
display (lm.unpooled)

# graph the data and fitted lines

x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)
y.range <- range (y[!is.na(match(county,display8))])

postscript ("c:/books/multilevel/radon1.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3,
        main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, add=TRUE)
}
dev.off()

# graph no-pooling ests vs. sample size

postscript ("c:/books/multilevel/radon2.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, coef(lm.unpooled)[-1], cex.lab=2.5, cex.axis=2.5,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (no pooling)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=2.5)
axis (2, seq(0,3), cex.axis=2.5)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    coef(lm.unpooled)[j+1] + c(-1,1)*se.coef(lm.unpooled)[j+1], lwd=.5)
}
dev.off()

# 3.  Multilevel model

# simplest mlm

M0 <- lmer (y ~ 1 + (1 | county))
display (M0)

# including x as a predictor

M1 <- lmer (y ~ x + (1 | county))
display (M1)
options (digits=2)
a.hat.M1 <- fixef(M1)[1] + ranef(M1)$county #coef(M1)[[1]]["(Intercept)"]
b.hat.M1 <- fixef(M1)[2]

# make the graphs

postscript ("c:/books/multilevel/radon3.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M1[j,] + b.hat.M1*x, lwd=1, col="black", add=TRUE)
}
dev.off()

# plot of ests & se's vs. sample size

a.se.M1 <- se.coef(M1)$county

postscript ("c:/books/multilevel/radon4.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, t(a.hat.M1), cex.lab=2.5, cex.axis=2.5,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (multilevel model)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=2.5)
axis (2, seq(0,3), cex.axis=2.5)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    a.hat.M1[j,] + c(-1,1)*a.se.M1[j], lwd=.5, col="gray10")
}
abline (coef(lm.pooled)[1], 0, lwd=.5)
dev.off()

# 4.  Multilevel model including uranium as a county-level predictor

# fit the model using lmer

u.full <- u[county]
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)
coef(M2)

a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + as.vector(ranef(M2)$county)
b.hat.M2 <- fixef(M2)[2]

# make the graphs

postscript ("c:/books/multilevel/radon5.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (a.hat.M1[j,] + b.hat.M1*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M2[j,] + b.hat.M2*x, lwd=1, col="black", add=TRUE)
}
dev.off()

# plot of ests & se's vs. county uranium

a.se.M2 <- se.coef(M2)$county

postscript ("c:/books/multilevel/radon6.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (u, t(a.hat.M2), cex.lab=2.5, cex.axis=2.5,
      xlab="county-level uranium measure", ylab="est. regression intercept", pch=20,
      ylim=c(0,1.5), yaxt="n", xaxt="n", mgp=c(3.5,1.2,0))
axis (1, seq(-1,1,.5), cex.axis=2.5, mgp=c(3.5,1.2,0))
axis (2, seq(0,1.5,.5), cex.axis=2.5, mgp=c(3.5,1.2,0))
curve (fixef(M2)["(Intercept)"] + fixef(M2)["u.full"]*x, lwd=1, col="black", add=TRUE)
for (j in 1:J){
  lines (rep(u[j],2), a.hat.M2[j,] + c(-1,1)*a.se.M2[j,], lwd=.5, col="gray10")
}
dev.off()

# 5.  Some examples of predictions using lmer

# new house in county 26 with x=1

x.squiggle <- 1
a.hat <- fixef(M2)[1] + fixef[3]*u + ranef(M2)
b.hat <- fixef(M2)[2]
sigma.y.hat <- sigma.hat(M2)$sigma$data

y.squiggle <- rnorm (1, a.hat[26] + b.hat*x.squiggle, sigma.y.hat)

n.sims <- 1000
y.squiggle <- rnorm (n.sims, a.hat[26] + b.hat*x.squiggle, sigma.y.hat)

# more complicated sims that we won't do
#
#sim.M1 <- sim (M1, n.sims)
#a.sim <- sim.M1$county[,,1]
#b.sim <- sim.M1$county[,,2]

quantile (y.squiggle, c(.25,.5,.75))

exp (quantile (y.squiggle, c(.25,.5,.75)))

unlogged <- exp (y.squiggle)
mean (unlogged)

# new house in a new county

u.squiggle <- mean (u)
g.0.hat <- fixef(M2)["(Intercept)"]
g.1.hat <- fixef(M2)["u.full"]
sigma.a.hat <- sigma.hat(M2)$sigma$county

a.squiggle <- rnorm (n.sims, g.0.hat + g.1.hat*u.squiggle, sigma.a.hat)
y.squiggle <- rnorm (n.sims, a.squiggle + b.hat*x.squiggle, sigma.y.hat)

quantile (y.squiggle, c(.25,.5,.75))

exp (quantile (y.squiggle, c(.25,.5,.75)))

# new house in county 2

y.squiggle <- rnorm (n.sims, a.hat[2] + b.hat*x.squiggle, sigma.y.hat)
quantile (y.squiggle, c(.25,.5,.75))

# avg for county 26
