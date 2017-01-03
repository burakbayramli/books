# The data came as an Excel file (ProfEvaltnsBeautyPublic.xls)
# I went into Excel and saved it as a .csv file (comma-separated version)

# Read the data into R, including the variable names (headers)
beauty.data <- read.table ("ProfEvaltnsBeautyPublic.csv", header=T, sep=",")

# Attach the data so they are accessible using their variable names
attach.all (beauty.data)

###############################################################################
# Do more beautiful profs get higher evaluations?
###############################################################################

# Rename the two variables for convenience
beauty <- btystdave
eval <- courseevaluation
n <- length (eval)

# Make a scatterplot
plot (beauty, eval)

# Make a scatterplot, labeling the axes
plot (beauty, eval, xlab="beauty", ylab="average teaching evaluation")

# Fit a linear regression
lm.1 <- lm.all (eval ~ beauty)

# Display the regression line, added onto the scatterplot (add=TRUE)
curve (lm.1$coef[1] + lm.1$coef[2]*x, add=T)

###############################################################################
# More models
###############################################################################

lm.4 <- lm.all (eval ~ beauty + female + age)

lm.5 <- lm.all (eval ~ beauty + female + minority)

lm.6 <- lm.all (eval ~ beauty + female + nonenglish)

lm.7 <- lm.all (eval ~ beauty + female + nonenglish + lower)

###############################################################################
# Go back to the simpler model, create posterior simulations
###############################################################################

lm.1 <- lm.all (eval ~ beauty)
ones <- rep (1, n)
X <- cbind (ones, beauty)
k <- ncol (X)
y <- eval

# Compute using the matrix formulas

beta.hat <- solve(t(X)%*%X) %*% t(X) %*% y
sigma.hat <- as.numeric (sqrt ((t(y-X%*%beta.hat) %*% (y-X%*%beta.hat))/(n-k)))
V.beta <- solve(t(X)%*%X)
sigma.sim <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
beta.sim <- mvrnorm (1, beta.hat, V.beta*sigma.sim^2)

# Plot a bunch of posterior simulated regression lines on the graph

for (i in 1:100){
  sigma.sim <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
  beta.sim <- mvrnorm (1, beta.hat, V.beta*sigma.sim^2)
  abline (beta.sim[1], beta.sim[2], col="gray")
}

###############################################################################
# Include indicators for the 30 courses (prelude to hierarchical model)
###############################################################################

# Set up the "design matrix":

X <- cbind (ones, beauty, class1, class2, class3, class4, class5, class6, class7, class8, class9, class10, class11, class12, class13, class14, class15, class16, class17, class18, class19, class20, class21, class22, class23, class24, class25, class26, class27, class28, class29, class30)

# Classical regression

beta.hat <- solve(t(X)%*%X) %*% t(X) %*% y
sigma.hat <- as.numeric (sqrt ((t(y-X%*%beta.hat) %*% (y-X%*%beta.hat))/(n-k)))
V.beta <- solve(t(X)%*%X)
sigma.sim <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
beta.sim <- mvrnorm (1, beta.hat, V.beta*sigma.sim^2)

beta.classical <- beta.hat

###############################################################################
# Bayesian regression with fixed prior distribution on the class effects
###############################################################################

# Put a N(mu,tau^2) prior distribution on class effects
# For simplicity, pick fixed values:  mu=0, tau=0.2

mu <- 0
tau <- 2
sigma <- sigma.hat  # sigma.hat, as estimated classically (0.508)

# Augment y

y.prior <- rep (mu, 30)
y.star <- c (y, y.prior)

# Augment X

zeroes <- rep (0, 30)
X.prior <- cbind (zeroes, zeroes, diag(30))
X.star <- rbind (X, X.prior)

# Augment Sigma.inverse

Sigma.inverse.classical <- diag(n)/sigma^2
Sigma.inverse.prior <- diag(30)/tau^2
Sigma.inverse.star <- diag(n+30)*0
Sigma.inverse.star[1:n,1:n] <- Sigma.inverse.classical
Sigma.inverse.star[(n+1):(n+30),(n+1):(n+30)] <- Sigma.inverse.prior

# Perform the regression on the augmented data
# (Computations are slightly different than before because there is
# no "sigma" parameter to be estimated)

beta.hat <- solve(t(X.star)%*%Sigma.inverse.star%*%X.star) %*%
  t(X.star) %*% Sigma.inverse.star %*% y.star
V.beta <- solve(t(X.star)%*%Sigma.inverse.star%*%X.star)
beta.sim <- mvrnorm (1, beta.hat, V.beta)

# summarize the results

plot(beta.classical[3:32],beta.hat[3:32], xlim=c(-1.5,.5), ylim=c(-1.5,.5))
abline(0,1)
plot(beta.classical[3:32],beta.sim[3:32], xlim=c(-1.5,.5), ylim=c(-1.5,.5))
abline(0,1)


###############################################################################
# Bayesian regression with fixed prior distribution on the class effects
###############################################################################

# Gibbs sampler:
#  (1) Given sigma, tau:  update beta using the above procedure (obtain beta.sim)
#  (2) Given beta, sigma:  update tau
#  (3) Given beta, tau:  update sigma
#
# Your homework for next week:
#  (a) Play around with the beauty data a bit.  Fit some models and discuss
#      the results.
#  (b) Write Gibbs sampler functions for steps (1), (2), (3).  You don't
#      have to program up the entire Gibbs sampler, just write these 3 R
#      functions (in the style of the "update" functions for the Gibbs sampler
#      on the bottom of page 601).

