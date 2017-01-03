# Radon computations for chapter 15 of the book

# 1.  No-pooling and complete-pooling models


# 2.  Multilevel model

# fit the model using bugs

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")
radon.1 <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", n.chains=3, n.iter=500)
plot (radon.1)
radon.1.noburnin <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", n.chains=3, n.iter=500, n.burnin=0)

# plot the bugs output

postscript ("c:/books/multilevel/radon.bugs.ps", horizontal=FALSE, height=6, width=6.5)
plot (radon.1)
dev.off ()

# plot showing gibbs convergence

postscript ("c:/books/multilevel/gibbs.converge.ps", height=6, horizontal=T)
par (mar=c(5,5,3,2)+.1)
plot (c(0,200), range(radon.1.noburnin$sims.array[6:208,,"mu.a"]), xlab="iteration",
      ylab="", xaxs="i", cex.lab=1.8, cex.axis=1.8, type="n")
mtext (expression(mu[alpha]), 2, 3, cex=2) 
for (j in 1:3){
  lines (1:203, radon.1.noburnin$sims.array[6:208,j,"mu.a"], lwd=.5)
}
dev.off()


# make some of the plots in chapter 12

a.pooled <- beta.hat(lm.pooled)[1]           # complete-pooling intercept
b.pooled <- beta.hat(lm.pooled)[2]           # complete-pooling slope
a.nopooled <- beta.hat(lm.unpooled)[2:(J+1)] # no-pooling vector of intercepts
b.nopooled <- beta.hat(lm.unpooled)[1]       # no-pooling slope

attach.bugs (radon.1)
a.multilevel <- rep (NA, J)
for (j in 1:J){
  a.multilevel[j] <- median (a[,j])}
b.multilevel <- median (b)

par (mfrow=c(2,4))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", main=uniq[j])
  curve (a.pooled + b.pooled*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (a.nopooled[j] + b.nopooled*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.multilevel[j] + b.multilevel*x, lwd=1, col="black", add=TRUE)
}

plot (sample.size.jittered, a.multilevel, xlab="sample size in county j",
  ylim=range(y), ylab=expression (paste ("intercept, ", alpha[j],
  "   (multilevel inference)")), pch=20, log="x")
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2), median(a[,j]) + c(-1,1)*sd(a[,j]))}
abline (a.pooled, 0, lwd=.5)

# simple prediction at end of Section 16.4

lqp.radon <- rep (NA, n.sims)
hennepin.radon <- rep (NA, n.sims)
for (s in 1:n.sims){
  lqp.radon[s] <- exp (rnorm (1, a[s,36] + b[s], sigma.y[s]))
  hennepin.radon[s] <- exp (rnorm (1, a[s,26] + b[s], sigma.y[s]))
}
diff <- lqp.radon - hennepin.radon
hist (diff)
print (mean(diff))
print (sd(diff))

# 3.  Multilevel model including uranium as a county-level predictor

# fit the model using bugs

radon.data <- list ("n", "J", "x", "y", "county", "u")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "g.0", "g.1", "sigma.y", "sigma.a")
radon.2 <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug", n.chains=3, n.iter=500)
plot (radon.2)


# 4.  Predictions

# these need to be fixed!!!

y.squiggle <- rnorm (1, a.hat[26] + b.hat[26], sigma.y)

y.squiggle <- rep (NA, n.sims)
for (s in 1:n.sims){
  y.squiggle <- rnorm (1, a.hat[26] + b.hat[26], sigma.y)
}

quantile (y.squiggle, c(.25,.5,.75))

unlogged <- exp (y.squiggle)
mean (unlogged)

# 5. Prediction using R

# new house in existing county

data.save <- save ("n", "y", "county", "x", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, 26)
x <- c (x, 1)
radon.parameters <- c ("a", "b", "g.0", "g.1", "sigma.y", "sigma.a")
radon.parameters <- c (radon.parameters, "y.squiggle")
radon.2a <- bugs (radon.data, radon.inits, radon.parameters, "radon.2a.bug", n.chains=3, n.iter=500)

# new house in new county

u.squiggle <- mean (u)

load ("radon.data")
save ("n", "y", "county", "x", "J", "u", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, J + 1)
x <- c (x, 1)
J <- J + 1
u <- c (u, u.squiggle)
radon.2a <- bugs (radon.data, radon.inits, radon.parameters, "radon.2a.bug", n.chains=3, n.iter=500)

# 6. Prediction using R

attach.bugs (radon.2)
y.squiggle <- rnorm (n.sims, a[,26] + b*1, sigma.y)

a.squiggle <- rnorm (n.sims, g.0 + g.1*u.squiggle, sigma.a)
y.squiggle <- rnorm (n.sims, a.squiggle + b*1, sigma.y)


# 7. Fake-data simulation

attach.bugs (radon.2)
b.true <- median (b)
g.0.true <- median (g.0)
g.1.true <- median (g.1)
sigma.y.true <- median (sigma.y)
sigma.a.true <- median (sigma.a)

a.true <- rep (NA, J)
for (j in 1:J){
  a.true[j] <- rnorm (1, g.0.true + g.1.true*u[j], sigma.a.true)}

y.fake <- rep (NA, n)
for (i in 1:n){
  y.fake[i] <- rnorm (1, a.true[county[i]] + b.true*x[i], sigma.y.true)}

radon.data <- list (n=n, J=J, y=y.fake, county=county, x=x, u=u)

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))}
radon.parameters <- c ("a", "b", "g.0", "g.1", "sigma.y", "sigma.a")
radon.2.fake <- bugs (radon.data, radon.inits, radon.parameters,
  "radon.2.bug", n.chains=3, n.iter=500)
print (radon.2.fake)

a.true[1] > quantile (a[,1], 0.25) & a.true[1] < quantile (a[,1], 0.75)

attach.bugs (radon.2.fake)
cover.50 <- rep (NA, J)
for (j in 1:J){
  cover.50[j] <- a.true[j] > quantile (a, 0.25) &
                 a.true[j] < quantile (a, 0.75)
}
mean (cover.50)


