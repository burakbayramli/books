#### Figure for Section 2, "Errors, In and Out of Sample"
# Example: linear regression through the origin, with Gaussian noise
# fix n, true slope, generate data, with X uniform on [0,1]
n<-20; theta<-5
x<-runif(n); y<-x*theta+rnorm(n)
# empirial risk = in-sample mean-squred error
empirical.risk <- function(b) { mean((y-b*x)^2) }
# generalization error of a line through the origin with slope b
  # EXERCISE: derive this formula
true.risk <- function(b) { 1 + (theta-b)^2*(0.5^2+1/12) }
# Plot the in-sample risk
curve(Vectorize(empirical.risk)(x),from=0,to=2*theta,
  xlab="regression slope",ylab="MSE risk")
  # R trickery: the empirical.risk() function, as written, does not behave well
  # when given a vector of slopes, and curve() wants its first argument to be a
  # function which can take a vector.  Vectorize() turns its argument into a
  # function which can take a vector; writing the expression
    # Vectorize(empirical.risk)(x)
  # rather than just
    # Vectorize(empirical.risk)
  # helps curve() figure out where to pass its vector of points.
curve(true.risk,add=TRUE,col="grey")
  # by contrast the true.risk() function works nicely with vectors.


#### Figures for Section 3, "Over-Fitting and Model Selection"
# Examples of training data
# 20 standard-Gaussian X's
x = rnorm(20)
# Quadratic Y's
y = 7*x^2 - 0.5*x + rnorm(20)

# Initial plot of training data plus true regression curve
plot(x,y)
curve(7*x^2-0.5*x,col="grey",add=TRUE)

# Fit polynomials and add them to the plot
# Fit a constant (0th order polynomial), plot it
y.0 = lm(y ~ 1)
   # We'd get the same constant by just doing mean(y), but fitting it as a
   # regression model means functions like residuals() and predict() are
   # available for use later, the same as our other models
abline(h=y.0$coefficients[1])
# Get evenly spaced points for pretty plotting of other models
d = seq(min(x),max(x),length.out=200)
# Fit polynomials of order 1 to 9
  # It would be nicer if we let this run from 0 to 9, but R doesn't allow us
  # to do a polynomial of degree 0
for (degree in 1:9) {
	fm = lm(y ~ poly(x,degree))
	# Store the results in models called y.1, y.2, through y.9
	  # The assign/paste trick here is often useful
	assign(paste("y",degree,sep="."), fm)
	# Plot them, with different line types
	lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}

# Calculate and plot in-sample errors
mse.q = vector(length=10)
for (degree in 0:9) {
	# The get() function is the inverse to assign()
	fm = get(paste("y",degree,sep="."))
	mse.q[degree+1] = mean(residuals(fm)^2)
}
plot(0:9,mse.q,type="b",xlab="polynomial degree",ylab="mean squared error",
     log="y")


# Plot the old curves with testing data
x.new = rnorm(2e4)
y.new = 7*x.new^2 - 0.5*x.new + rnorm(2e4)
plot(x.new,y.new,xlab="x",ylab="y",pch=24,cex=0.1,col="blue")
curve(7*x^2-0.5*x,col="grey",add=TRUE)
abline(h=y.0$coefficients[1])
d = seq(from=min(x.new),to=max(x.new),length.out=200)
for (degree in 1:9) {
	fm = get(paste("y",degree,sep="."))
	lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}
points(x,y)


# Calculate and plot the out-of-sample errors
gmse.q = vector(length=10)
for (degree in 0:9) {
	# The get() function is the inverse to assign()
	fm = get(paste("y",degree,sep="."))
	predictions = predict(fm,data.frame(x=x.new))
	resids = y.new - predictions
	gmse.q[degree+1] = mean(resids^2)
}
plot(0:9,mse.q,type="b",xlab="polynomial degree",
     ylab="mean squared error",log="y",ylim=c(min(mse.q),max(gmse.q)))
lines(0:9,gmse.q,lty=2,col="blue")
points(0:9,gmse.q,pch=24,col="blue")



### Figures for section 4, "Cross-Validation"
housing <- read.csv("http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/01/calif_penn_2011.csv")

# Divide the data randomly into two (nearly) equal halves
half_A <- sample(1:nrow(housing),size=nrow(housing)/2,replace=FALSE)
half_B <- setdiff(1:nrow(housing),half_A)
# Write out the formulas for our two linear model specifications just once
small_formula = "Median_house_value ~ Median_household_income"
large_formula = "Median_house_value ~ Median_household_income + Median_rooms"
small_formula <- as.formula(small_formula)
large_formula <- as.formula(large_formula)
# Fit each model specification to each half of the data
mAsmall <- lm(small_formula,data=housing,subset=half_A)
mBsmall <- lm(small_formula,data=housing,subset=half_B)
mAlarge <- lm(large_formula,data=housing,subset=half_A)
mBlarge <- lm(large_formula,data=housing,subset=half_B)
  # EXERCISE: Extract the coefficients for all the models
# Calculating the in-sample MSE is a repeated task, so write a function for it
in.sample.mse <- function(model) { mean(residuals(model)^2) }
in.sample.mse(mAsmall); in.sample.mse(mAlarge)
in.sample.mse(mBsmall); in.sample.mse(mBlarge)
# Calculating the MSE of a model on new data also deserves a function
new.sample.mse <- function(model,half) {
   test <- housing[half,]
   predictions <- predict(model,newdata=test)
   return(mean((test$Median_house_value - predictions)^2))
}
  # EXERCISE: is in.sample.mse(mAsmall) == new.sample.mse(mAsmall,half_A) ?
  # EXERCISE: should they be equal?
new.sample.mse(mAsmall,half_B); new.sample.mse(mBsmall,half_A)
new.sample.mse(mBsmall,half_A); new.sample.mse(mASmall,half_B)
