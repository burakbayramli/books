## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots

# The R codes & data files should be saved in the same directory for
# the source command to work

source("13.5_Non-nested models.R") # where data was cleaned

## Define variables
treatment <- group.id
airport <- scenario.id

## Fit the 2-model using Bugs
n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), sigma.d.raw=runif(1), sigma.y=runif(1),
  mu.g.raw=rnorm(1), mu.d.raw=rnorm(1), sigma.g.raw=runif(1), xi.d=rnorm(1),
  xi.g=runif(1))
}

parameters <- c("mu", "sigma.y", "sigma.g", "sigma.d")

pilots <- bugs (data, inits, parameters, "pilots.19.5.bug", n.chains=3, 
   n.iter=1000, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )


