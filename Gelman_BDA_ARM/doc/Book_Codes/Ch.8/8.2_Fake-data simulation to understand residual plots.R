## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/simulation

library ("arm")

grades <- read.table ("gradesW4315.dat", header=TRUE)
midterm <- grades[,"Midterm"]
final <- grades[,"Final"]

## Estimate the model

lm.1 <- lm (final ~ midterm)
display (lm.1)

## Construct fitted values

n <- length(final)
X <- cbind (rep(1,n), midterm)
predicted <- X %*% coef (lm.1)
resid <- lm.1$residuals

## Simulate fake data & compute fitted values

a <- 65
b <- 0.7
sigma <- 15
y.fake <- a + b*midterm + rnorm (n, 0, sigma)

lm.fake <- lm (y.fake ~ midterm)
predicted.fake <- X %*% coef (lm.fake)
resid.fake <- y.fake - predicted.fake

par (mfrow=c(2,2))

## Plots figure 8.1

 # plot on the left

plot (predicted, resid, xlab="predicted value", ylab="residual",
  main="Residuals vs. predicted values", pch=20)
abline (0,0, col="gray", lwd=.5)

 # plot on the right

plot (final, resid, xlab="observed value", ylab="residual",
  main="Residuals vs. observed values", pch=20)
abline (0,0, col="gray", lwd=.5)

## Plots figure 8.2

 # plot on the left

plot (predicted.fake, resid.fake, xlab="predicted value", ylab="residual",
  main="Fake data: resids vs. predicted", pch=20)
abline (0,0, col="gray", lwd=.5)

 # plot on the right

plot (y.fake, resid.fake, xlab="observed value", ylab="residual",
  main="Fake data: resids vs. observed", pch=20)
abline (0,0, col="gray", lwd=.5)
