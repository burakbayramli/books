# R code for entering the data and fitting the Bugs model for 8 schools
# analysis from Section 5.5 of "Bayesian Data Analysis".

# To run, the Bugs model must be in the file "schools.txt" in your working
# directory and you must load in the functions in the bugs.R file (see
# http://www.stat.columbia.edu/~gelman/bugsR/).

J <- 8
y <- c(28,8,-3,7,-1,1,18,12)
sigma.y <- c(15,10,16,11,9,11,10,18)
schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
schools.parameters <- c("theta", "mu.theta", "sigma.theta")

# run in winbugs14

schools.sim <- bugs (schools.data, schools.inits, schools.parameters, "schools.bug", n.chains=3, n.iter=1000)

# display the results on console and graphically

print (schools.sim)
plot (schools.sim)

# run in openbugs

# This is not set up yet!

