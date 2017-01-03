# In-class demos for the lecture on the bootstrap, 36-402, Spring 2012


library(MASS)
data(cats)
summary(cats)

# Is Moonshine over-weight?
  # Find 95th percentile of weight, assuming a Gaussian distribution of body
  # weights, matched to data
(q95.gaussian <- qnorm(0.95,mean=mean(cats$Bwt),sd=sd(cats$Bwt)))
### How precisely do I know this?
# Simulate from the fitted Gaussian
rcats.gaussian <- function() {
  rnorm(n=nrow(cats),mean=mean(cats$Bwt),sd=sd(cats$Bwt))
}
# Replicate the initial estimation on some surrogate data
est.q95.gaussian <- function(x) {
  m <- mean(x)
  s <- sd(x)
  return(qnorm(0.95,mean=m,sd=s))
}
  # R exercise for the reader: convince yourself that this works properly
  # on the original data set

# Draw 1000 surrogate data sets and repeat the estimation on each to get
# an approximation to the sampling distribution
sampling.dist.gaussian <- replicate(1000, est.q95.gaussian(rcats.gaussian()))
# Plot the sampling distribution: histogram, smoothed density estimate
plot(hist(sampling.dist.gaussian,breaks=50),freq=FALSE)
plot(density(sampling.dist.gaussian))
abline(v=q95.gaussian,lty=2)  # Add original value

# Find measures of uncertainty from the 1000 bootstrap replicates
# Standard error of the estimate:
sd(sampling.dist.gaussian)
# 95% confidence interval for estimate (crude):
quantile(sampling.dist.gaussian,c(0.025,0.975))
# Better confidence intervals (see notes):
2*q95.gaussian - quantile(sampling.dist.gaussian,c(0.975,0.025))

# Is the Gaussian distribution assumption a good one?
plot(hist(cats$Bwt),freq=FALSE)
lines(density(cats$Bwt),lty=2)
curve(dnorm(x,mean=mean(cats$Bwt),sd=sd(cats$Bwt)),add=TRUE,col="purple")
# Not very bell-curve looking.

# Is Moonshine over-weight?, take two
# Direct or empirical, non-parametric estimate of 95th percentile
(q95.np <- quantile(cats$Bwt,0.95))
# How uncertain is this?
resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}
est.q95.np <- function(x) {
  quantile(x,0.95)
}
  # R exercise for the reader: convince yourself that this works properly
  # on the original data set
sampling.dist.np <- replicate(1000, est.q95.np(resample(cats$Bwt)))
plot(density(sampling.dist.np))
abline(v=q95.np,lty=2)
# Standard error of the estimate:
sd(sampling.dist.np)
# Bias of the estimate:
mean(sampling.dist.np - q95.np)
# Crude 95% CI:
quantile(sampling.dist.np,c(0.025,0.975))
# More refined CI:
2*q95.np - quantile(sampling.dist.np,c(0.975,0.025))

# Relating heart weight (in grams) to body weight (in kilograms):
plot(Hwt~Bwt, data=cats, xlab="Body weight (kg)", ylab="Heart weight (g)")
cats.lm <- lm(Hwt ~ Bwt, data=cats)
abline(cats.lm)
coefficients(cats.lm)
confint(cats.lm)
plot(cats$Bwt,residuals(cats.lm))
plot(density(residuals(cats.lm)))
# CIs by resampling cases:
# First re-fit on a subset of the data
coefs.cats.lm <- function(subset) {
  fit <- lm(Hwt~Bwt,data=cats,subset=subset)
  return(coefficients(fit))
}
  # R exercise for the reader: convince yourself that this works properly
  # when given all the rows
  # R exercise for the reader: convince yourself that if the subset vector
  # contains the same index multiple times, this actually, and appropriately,
  # changes the result

# Now do on many random subsets
cats.lm.sampling.dist <- replicate(1000, coefs.cats.lm(resample(1:nrow(cats))))
(limits <- apply(cats.lm.sampling.dist,1,quantile,c(0.025,0.975)))
  # Noticeably broader
  # R exercise for the reader: get the more refined limits out of this
