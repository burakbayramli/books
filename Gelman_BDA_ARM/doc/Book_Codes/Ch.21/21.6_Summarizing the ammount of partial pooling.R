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
radon.parameters <- c (radon.parameters, "e.a")

radon.ch21.6a <- bugs (radon.data, radon.inits, radon.parameters, "radon.ch21.6a.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )

attach.bugs (radon.ch21.6a)

omega <- (sd(e.a)/sigma.a)^2
omega <- pmin (omega, 1)

## Summary pooling factor for each batch of parameters
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a")
radon.parameters <- c (radon.parameters, "e.a", "e.y")

radon.ch21.6b <- bugs (radon.data, radon.inits, radon.parameters, "radon.ch21.6b.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )

attach.bugs (radon.ch21.6b)

lambda.y <- 1 - var (apply (e.y, 2, mean))/ mean (apply (e.y, 1, var))
lambda.a <- 1 - var (apply (e.a, 2, mean))/ mean (apply (e.a, 1, var))

# if slope varies
lambda.b <- 1 - var (apply (e.b, 2, mean))/ mean (apply (e.b, 1, var))

