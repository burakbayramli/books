## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.2_Partial pooling with no predictors.R") # where data was cleaned
# close the Bugs window to proceed

## Code for calling the "modeling the correlation" Bugs code
radon.data <- list ("n", "J", "x", "y", "county", "u")
radon.inits <- function (){
  list (B=array (rnorm (2*J), c(J,2)), g.a.0=rnorm(1), g.a.1=rnorm(1),
    g.b.0=rnorm(1), g.b.1=rnorm(1),
    sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}
radon.parameters <- c ("a", "b", "g.a.0", "g.a.1", "g.b.0", "g.b.1",
  "sigma.y", "sigma.a", "sigma.b", "rho")
radon.bugs.4a <- bugs (radon.data, radon.inits, radon.parameters, "radon.4a.bug", 
             n.chains=3, n.iter=10000, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

windows ()
plot (radon.bugs.4a)

## Set up and call Bugs for the scaled-inverse Wishart distribution

library("MCMCpack")

W <- diag (2)
radon.data <- list ("n", "J", "x", "y", "county", "W")

radon.inits <- function (){
  list (B.raw=array(rnorm(2*J), c(J,2)), mu.a.raw=rnorm(1), mu.b.raw=rnorm(1),
    sigma.y=runif(1), Tau.B.raw=rwish(3,diag(2)), xi.a=runif(1), 
    xi.b=runif(1))
}

radon.parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b",
    "rho")

M1 <- bugs (radon.data, radon.inits, radon.parameters, "wishart1.bug",
    n.chains=3, n.iter=2000, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )

## Set up and call Bugs for the multiple varying coefficients model

library("MCMCpack")

X <- cbind (1,x)
K <- ncol (X)
W <- diag (K)
radon.data <- list ("n", "J", "K", "X", "y", "county", "W")

radon.inits <- function (){
  list (B.raw=array(rnorm(J*K), c(J,K)), mu.raw=rnorm(K), sigma.y=runif(1),
       Tau.B.raw=rwish(K+1,diag(K)), xi=runif(K))
}

radon.parameters <- c ("B", "mu", "sigma.y", "sigma.B", "rho.B")

M2 <- bugs (radon.data, radon.inits, radon.parameters, "wishart2.bug",
    n.chains=3, n.iter=2000, bugs.directory="c:/...", working.directory=NULL, 
    clearWD=TRUE, debug=TRUE )


