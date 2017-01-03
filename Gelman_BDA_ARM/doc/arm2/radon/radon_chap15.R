# Radon computations for chapter 15 of the book

# 1.  No-pooling and complete-pooling models


# 2.  Multilevel model

# fit the model using bugs

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")
radon.bugs.1 <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", n.chains=3, n.iter=10)
windows ()
plot (radon.bugs.1)

attach.bugs (radon.bugs.1)

# 3.  Multilevel model including uranium as a county-level predictor

# fit the model using bugs

radon.data <- list ("n", "J", "x", "y", "county", "u")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "g.0", "g.1", "sigma.y", "sigma.a")
radon.bugs.2 <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug", n.chains=3, n.iter=10)
windows ()
plot (radon.bugs.2)


# 4.  Predictions

# these need to be fixed!!!

y.squiggle <- rnorm (1, a.hat[26] + b.hat[26], sigma.y)

y.squiggle <- rep (NA, n.sims)
for (s in 1:n.sims){
  y.squiggle <- rnorm (1, a.hat[26] + b.hat[26], sigma.y)
}

quantile (y.squiggle, c(.25,.5,.75))

unlogged <- exp (y.squiggle)
mean (unlogged)
