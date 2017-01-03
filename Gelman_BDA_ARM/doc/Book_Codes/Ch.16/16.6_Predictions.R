## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.6_Group-level predictors.R") # where variables were defined
# close the Bugs window to proceed

## Extend the dataset
radon.data <- list ("n", "J", "x", "y", "county", "u")
data.save <- save ("n", "y", "county", "x", "u", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, 26)
x <- c (x, 1)
u <- c (u, 1)

## Fit the model
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a")
radon.parameters <- c (radon.parameters, "y.tilde")

radon.2a <- bugs (radon.data, radon.inits, radon.parameters, "radon.2a.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

attach.bugs (radon.2a)
quantile (exp (y.tilde), c(.25, .75))

## New unit in a new group
u.tilde <- mean (u)

load ("radon.data")
data.save <- save ("n", "y", "county", "x", "J", "u", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, J+1)
x <- c (x, 1)
J <- J + 1
u <- c (u, u.tilde)

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")
radon.parameters <- c (radon.parameters, "y.tilde")

radon.2a <- bugs (radon.data, radon.inits, radon.parameters, "radon.2a.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

attach.bugs (radon.2a)
quantile (exp (y.tilde), c(.25, .75))

## Prediction using R
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2 <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

attach.bugs (radon.2)

y.tilde <- rnorm (n.sims, a[,26] + b*1, sigma.y)

a.tilde <- rnorm (n.sims, g.0 + g.1*u.tilde, sigma.a)
y.tilde <- rnorm (n.sims, a.tilde + b*1, sigma.y)


