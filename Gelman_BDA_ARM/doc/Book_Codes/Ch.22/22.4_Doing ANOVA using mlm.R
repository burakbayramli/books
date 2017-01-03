## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.2_Partial pooling with no predictors.R") # where data was cleaned
# close the Bugs window to proceed

## Fit the model

radon.data <- list ("n", "J", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a", "s.y", "s.a")

anova.radon.nopred <- bugs (radon.data, radon.inits, radon.parameters, 
 "anova.radon.nopred.bug", n.chains=3, n.iter=2000, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )
attach.bugs (anova.radon.nopred)
