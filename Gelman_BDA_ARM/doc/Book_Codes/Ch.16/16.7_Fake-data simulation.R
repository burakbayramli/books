## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.6_Group-level predictors.R") # where variables were defined
# close the Bugs window to proceed

## Set up and call Bugs
radon.data <- list ("n", "J", "x", "y", "county", "u")

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2 <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

attach.bugs (radon.2)

## Specifying the unmodeled parameters
b.true <- median (b)
g.0.true <- median (g.0)
g.1.true <- median (g.1)
sigma.y.true <- median (sigma.y)
sigma.a.true <- median (sigma.a)

## Simulating the varying coefficients
a.true <- rep (NA, J)
for (j in 1:J){
  a.true[j] <- rnorm (1, g.0.true + g.1.true*u[j], sigma.a.true)
}

## Simulating fake data
y.fake <- rep (NA, n)
for (i in 1:n){
  y.fake[i] <- rnorm (1, a.true[county[i]] + b.true*x[i], sigma.y.true)
}

## Inference and comparison to "true" values

 # specify the data
radon.data <- list (n=n, J=J, y=y.fake, county=county, x=x, u=u)

 # specify the rest of inputs

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2.fake <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug",
    n.chains=3, n.iter=500, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

print (radon.2.fake)

## Checking coverage of 50% intervals
attach (radon.2.fake)

 # coverage for alpha1
a.true[1] > quantile (a[1,], .25) & a.true[1] < quantile (a[1,], .75)

 # coverage for the 85 alphas
cover.50 <- rep (NA, J)
for (j in 1:J){
  cover.50[j] <- a.true[j] > quantile (a, .25) & 
                 a.true[j] < quantile (a, .75)
}
mean (cover.50)





