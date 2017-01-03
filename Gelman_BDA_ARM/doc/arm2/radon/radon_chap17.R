# Radon computations for chapter 16 of the book

# 1.  varying-intercept, varying-slope with no group-level predictors


# 2.  varying-intercept, varying-slope with uranium as a group-level predictor


# fit model using bugs

radon.data <- list ("n", "J", "x", "y", "county", "u")
radon.inits <- function (){
  list (B=array (rnorm (2*J), c(J,2)), g.a.0=rnorm(1), g.a.1=rnorm(1),
    g.b.0=rnorm(1), g.b.1=rnorm(1),
    sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}
radon.parameters <- c ("a", "b", "g.a.0", "g.a.1", "g.b.0", "g.b.1",
  "sigma.y", "sigma.a", "sigma.b", "rho")
radon.bugs.4a <- bugs (radon.data, radon.inits, radon.parameters, "radon.4a.bug", n.chains=3, n.iter=10000)
windows ()
plot (radon.bugs.4a)

