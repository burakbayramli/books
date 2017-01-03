## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.6_Group-level predictors.R") # where variables were defined
# close the Bugs window to proceed

# varying-intercept model (NO FLOOR!)
model.1 <- lmer (y ~ u.full + (1 | county))
display (model.1)

# add floor as an individual-level predictor
model.2 <- lmer (y ~ u.full + x + (1 | county))
display (model.2)

# add houses with no basement as group-level predictor
x.mean <- rep (NA, J)
for (j in 1:J){
  x.mean[j] <- mean(x[county==j])
}
x.mean.full <- x.mean[county]

model.3 <- lmer (y ~ u.full + x + x.mean.full + (1 | county))
display (model.3)

# Figure 21.9
par (mfrow=c(1,3), mar=c(4,6,4,1.8), oma=c(0,0,0,0))
b.00 <- -2
plot (c(0,1), c(-2,3.5), type="n", xlab="floor", ylab="log radon level",
      cex.lab=1.2, cex.axis=1.2, main="naturally low-radon county",
      mgp=c(2.8,.7,0), xaxt="n", yaxt="n", cex.main=1.2)
axis (1, c(0,1), mgp=c(2.8,.7,0), cex.axis=1.2)
axis (2, seq(-1,3,2), mgp=c(2.8,.7,0), cex.axis=1.2)
a.00 <- 1
rand1 <- rnorm(2,0,1)
rand1 <- (rand1-mean(rand1))/sd(rand1)
rand2 <- rnorm(18,0,1)
rand2 <- (rand2-mean(rand2))/sd(rand2)
points (rep(c(1,0),c(2,18)), a.00 + rep(c(1,0),c(2,18))*b.00 + c(rand1,rand2)*0.8)
curve (a.00 + b.00*x, lwd=.5, add=TRUE)

plot (c(0,1), c(-2,3.5), type="n", xlab="floor", ylab="log radon level",
      cex.lab=1.2, cex.axis=1.2, main="intermediate county",
      mgp=c(2.8,.7,0), xaxt="n", yaxt="n", cex.main=1.2)
axis (1, c(0,1), mgp=c(2.8,.7,0), cex.axis=1.2)
axis (2, seq(-1,3,2), mgp=c(2.8,.7,0), cex.axis=1.2)
a.00 <- 1.8
rand1 <- rnorm(10,0,1)
rand1 <- (rand1-mean(rand1))/sd(rand1)
rand2 <- rnorm(10,0,1)
rand2 <- (rand2-mean(rand2))/sd(rand2)
points (rep(c(1,0),c(10,10)), a.00 + rep(c(1,0),c(10,10))*b.00 + c(rand1,rand2)*0.8)
curve (a.00 + b.00*x, lwd=.5, add=TRUE)

plot (c(0,1), c(-2,3.5), type="n", xlab="floor", ylab="log radon level",
      cex.lab=1.2, cex.axis=1.2, main="naturally high-radon county",
      mgp=c(2.8,.7,0), xaxt="n", yaxt="n", cex.main=1.2)
axis (1, c(0,1), mgp=c(2.8,.7,0), cex.axis=1.1)
axis (2, seq(-1,3,2), mgp=c(2.8,.7,0), cex.axis=1.2)
a.00 <- 2.6
rand1 <- rnorm(18,0,1)
rand1 <- (rand1-mean(rand1))/sd(rand1)
rand2 <- rnorm(2,0,1)
rand2 <- (rand2-mean(rand2))/sd(rand2)
points (rep(c(1,0),c(18,2)), a.00 + rep(c(1,0),c(18,2))*b.00 + c(rand1,rand2)*0.8)
curve (a.00 + b.00*x, lwd=.5, add=TRUE)

