# In-class examples for 19 February 2013

# Testing a linear regression specification against the non-parametric
# alternative

# First case: linear specification is wrong

# Make up sample data
x <- runif(300,0,3)
# Impose true regression function log(x+1), with Gaussian noise
yg <- log(x+1)+rnorm(length(x),0,0.15)
# Bind into a data frame
gframe <- data.frame(x=x,y=yg)
# Plot it
plot(y~x, data=gframe,xlab="x",ylab="y")
# Add the true regression curve
curve(log(1+x),col="grey",add=TRUE)

# Fit the linear model, add to the plot
glinfit = lm(y~x,data=gframe)
print(summary(glinfit),signif.stars=FALSE,digits=2)
plot(x,yg,xlab="x",ylab="y")
curve(log(1+x),col="grey",add=TRUE,lwd=4)
abline(glinfit,lwd=4)
# MSE of linear model, in-sample
mean(residuals(glinfit)^2)

# We'll need to do that a lot, make it a function
mse.residuals <- function(model) {
  mean(residuals(model)^2)
}
# EXERCISE: Write comments giving the inputs and outputs of the function
# EXERCISE: Check that the function works

# Fit the non-parametric alternative
library(mgcv)
# We'll use spline smoothing as provided by the gam() function
gnpr <- gam(y~s(x),data=gframe)
# Sort the x values in increasing order for plotting
x.ord <- order(x)
# Add the fitted values from the spline to the plot
lines(x[x.ord],fitted(gnpr)[x.ord],col="blue")

# Calculate the difference in MSEs
t.hat = mse.residuals(glinfit) - mse.residuals(gnpr)
  # EXERCISE: Why do we believe this is the right number?

# Simulate from the parametric model, assuming Gaussian noise
# EXERCISE: Write comments describing input and output of this function
sim.lm <- function(linfit, test.x) {
  n <- length(test.x)
  sim.frame <- data.frame(x=test.x)
  sigma <- sqrt(mse.residuals(linfit))  # There are other ways to get sigma
  y.sim <- predict(linfit,newdata=sim.frame)
  y.sim <- y.sim + rnorm(n,0,sigma)
  sim.frame <- data.frame(sim.frame,y=y.sim)
  return(sim.frame)
}
# EXERCISE: How can we check that this is working?

# Calculate difference between parametric and nonparametric models on a
# data frame
# EXERCISE: More comments
# EXERCISE: Check this function
calc.T <- function(data) {
  MSE.p <- mse.residuals(lm(y~x,data=data))
  MSE.np <- mse.residuals(gam(y~s(x),data=data))
  return(MSE.p - MSE.np)
}

# Calculate the MSE difference on one simulation run
calc.T(sim.lm(glinfit,x))
# Calculate the MSE difference on 200 simulation runs, so we get a sample
# from the null hypothesis
null.samples.T <- replicate(200,calc.T(sim.lm(glinfit,x)))
# How often does the simulation produce gaps bigger than what we really saw?
sum(null.samples.T > t.hat)

# Plot histogram of the sampling distribution, and the observed value
hist(null.samples.T,n=31,xlim=c(min(null.samples.T),1.1*t.hat),probability=TRUE)
abline(v=t.hat)



# Second case: linear model is properly specified
  # Mostly left uncommented so that you can explore
y2 = 0.2+0.5*x + rnorm(length(x),0,0.15)
  # Why don't I have to make up x values again?
y2.frame <- data.frame(x=x,y=y2)
plot(x,y2,xlab="x",ylab="y")
abline(0.2,0.5,col="grey")
y2.fit <- lm(y~x,data=y2.frame)
null.samples.T.y2 <- replicate(200,calc.T(sim.lm(y2.fit,x)))
t.hat2 <- calc.T(y2.frame)
hist(null.samples.T.y2,n=31,probability=TRUE)
abline(v=t.hat2)