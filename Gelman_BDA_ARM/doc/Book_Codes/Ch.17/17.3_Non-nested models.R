## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots

# The R codes & data files should be saved in the same directory for
# the source command to work

source("13.5_Non-nested models.R") # where data was cleaned

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Fit the 2-model using Bugs

n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), sigma.delta=runif(1), sigma.gamma=runif(1), sigma.y=runif(1))
}

parameters <- c("mu", "sigma.y", "sigma.gamma", "sigma.delta")

pilots <- bugs (data, inits, parameters, "pilots.bug", n.chains=3, n.iter=1000,
     bugs.directory="c:/...", working.directory=NULL, clearWD=TRUE, debug=TRUE )










