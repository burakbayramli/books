## Read & clean the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

library ("arm")
heights <- read.dta ("heights.dta")
attach.all (heights)

  # define variables 
age <- 90 - yearbn                     # survey was conducted in 1990
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

  # (for simplicity) remove cases with missing data
ok <- !is.na (earn+height+sex) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age,
    age.category, eth, male)[ok,])
n <- nrow (heights.clean)
attach.all (heights.clean)
height.jitter.add <- runif (n, -.2, .2)

 # rename variables
y <- log(earn)
x <- height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- age.category

## Regression of log (earnings) on height, age, and ethnicity
M1 <- lmer (y ~ x + (1 + x | eth))
display (M1)
ab.hat.M1 <- coef(M1)$eth
a.hat.M1 <- ab.hat.M1[,1]
b.hat.M1 <- ab.hat.M1[,2]

 # plots on figure 13.3
x.jitter <- x + runif(n, -.2,.2)
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

               ### ???? ####
ab.sim.M1 <- sim(M1)$eth
a.sim.M1 <- ab.sim.M1[,,1]
b.sim.M1 <- ab.sim.M1[,,2]
                ### ???? ####

## Figure 13.3 ?
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

## Figure 13.3 using BUGS
x <- height - mean(height)
x.jitter.add <- runif(n, -.2,.2)

data <- list ("n", "x", "y", "n.eth", "eth")
inits <- function(){
  list (B=array(rnorm(n.eth*2), c(n.eth,2)), sigma.y=runif(1),
        mu.a=rnorm(1), mu.b=runif(1),
        sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}
parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y",
                 "sigma.a", "sigma.b", "rho")

M2.bugs <- bugs (data, inits, parameters, "earnings1.bug", n.chains=3, n.iter=10000,
        bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )
attach.bugs (M2.bugs)

a <- a - b*mean(height)
a.hat <- apply (a, 2, mean)
a.se <- apply (a, 2, sd)
b.hat <- apply (b, 2, mean)
b.se <- apply (b, 2, sd)

 # plot
x <- height - mean(height)
par (mfrow=c(2,2), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot ((x + x.jitter.add)[eth==j], y[eth==j], xlab="height (inches)", ylab="log earnings",
      mgp=c(2,.7,0), xlim=range(x), ylim=range(y), yaxt="n", cex.lab=1.2, cex.axis=1.1, pch=20, 
      cex=.6, cex.main=1.2, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.1, mgp=c(2,.5,0))
  for (s in 1:20){
    curve (a[s,j] + b[s,j]*x, lwd=.5, col="gray20", add=TRUE)
  }
  curve (a.hat[j] + b.hat[j]*x, lwd=1, add=TRUE)
}

## plot on figure 13.4
par (mar=c(7,8,4,2)+.1)
plot (a.hat.M1, b.hat.M1,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=1.1, cex.lab=1.2, cex=1.1, mgp=c(5.5,1.5,0), type="n", yaxt="n")
axis (2, seq(0.03,0.06,0.01), cex.axis=1.1, mgp=c(4.5, 1.5,0))
text (a.hat.M1, b.hat.M1, substr (eth.label,1,1), cex=1.1)

 # plot on figure 13.5
par (mar=c(5,5,4,2)+.1)
plot (x.jitter, y, xlim=c(0,80), xlab="height (inches)", ylab="log earnings",
      cex.lab=1.1, cex.axis=1.1, xaxs="i", pch=20, cex=1.1, yaxt="n")
axis (2, seq(6,12,2), cex.axis=1.1)
for (j in 1:4){
  curve (a.hat.M1[j] + b.hat.M1[j]*x, add=TRUE)
}
adj1 <- c(-3, 2, -3, 0)
adj2 <- c(.3, -.4, .4, +.4)
adj1 <- c(-3,0,0,3)
adj2 <- c(.2,0,0,-.2)
text (20 + adj1, a.hat.M1 + b.hat.M1*20 + adj2, eth.label, cex=1)

## Regression centering the predictors
x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)

M2 <- lmer (y ~ x.centered + (1 + x.centered | eth))
display (M2)

ab.hat.M2 <- coef(M2)$eth
a.hat.M2 <- ab.hat.M2[,1]
b.hat.M2 <- ab.hat.M2[,2]

## Figure 13.6
x.jitter <- x + runif(n, -.2,.2)
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

               ### ???? ####
ab.sim.M2 <- sim(M2)$eth
a.sim.M2 <- ab.sim.M2[,,1]
b.sim.M2 <- ab.sim.M2[,,2]
                ### ???? ####

par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot (x.centered.jitter[eth==j], y[eth==j], xlab="height (inches from mean)", ylab="log earnings", mgp=c(2,.7,0), xlim=range(x.centered), ylim=range(y),
        yaxt="n",
        cex.lab=1.1, cex.axis=1.3, pch=20, cex=.6, cex.main=1.2, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.1, mgp=c(2,.5,0))
  for (i in 1:20){
    curve (a.sim.M2[i,j] + b.sim.M2[i,j]*x, lwd=.5, col="gray", add=TRUE)
  }
  curve (a.hat.M2[j] + b.hat.M2[j]*x, lwd=1, add=TRUE)
}

## Figure 13.6 using BUGS
attach.bugs (M2.bugs)
x <- height - mean(height)
a.hat <- apply (a, 2, mean)
a.se <- apply (a, 2, sd)
b.hat <- apply (b, 2, mean)
b.se <- apply (b, 2, sd)

par (mfrow=c(2,2), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot ((x + x.jitter.add)[eth==j], y[eth==j], xlab="height (inches from mean)", ylab="log earnings", 
        mgp=c(2,.7,0), xlim=range(x), ylim=range(y), yaxt="n", cex.lab=1.1, cex.axis=1.1, pch=20,
        cex=.6, cex.main=1.2, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.1, mgp=c(2,.5,0))
  for (s in 1:20){
    curve (a[s,j] + b[s,j]*x, lwd=.5, col="gray20", add=TRUE)
  }
  curve (a.hat[j] + b.hat[j]*x, lwd=1, add=TRUE)
}

## Figure 13.7
par (mar=c(7,8,4,2)+.1)
plot (a.hat.M2, b.hat.M2,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=1.1, cex.lab=1.1, cex=1.1, mgp=c(5.5,1.5,0), type="n", yaxt="n")
axis (2, seq(0.03,0.06,0.01), cex.axis=1.1, mgp=c(4.5, 1.5,0))
text (a.hat.M2, b.hat.M2, substr (eth.label,1,1), cex=1)


