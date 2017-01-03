## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

source("12.3_Partial pooling with predictors.R") # where variables were defined
# close the Bugs window to proceed

## Redundant mean parameters for a simple nested model
radon.data <- list ("n", "y", "J", "county")

radon.inits <- function(){
  list (mu=rnorm(1), mu.eta=rnorm(1), eta=rnorm(J), sigma.y=runif(1),
        sigma.eta=runif(1))
}

radon.parameters <- c ("mu.adj", "eta.adj", "sigma.y", "sigma.eta")

fit.4 <- bugs (radon.data, radon.inits, radon.parameters, "M4.bug", n.chains=3, 
  n.iter=100, bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, 
  debug=TRUE )
plot(fit.4)

#############################################################################################
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
  list (mu=rnorm(1), mu.g=rnorm(1), mu.d=rnorm(1), sigma.d=runif(1), sigma.g=runif(1), sigma.y=runif(1))
}

parameters <- c("mu.adj", "g.adj", "d.adj", "sigma.y", "sigma.g", "sigma.d")

pilots.fit <- bugs (data, inits, parameters, "pilots.ch19.bug", n.chains=3, n.iter=500,
     bugs.directory="c:/Arquivos de programas/WinBUGS14/", working.directory=NULL, clearWD=TRUE, debug=TRUE )
plot(pilots.fit)

#############################################################################################
## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88

# The R codes & data files should be saved in the same directory for
# the source command to work

source("17.4_Multilevel logistic regression.R") # where variables were defined
# don't matter the bugs error message; model will be different

## Multilevel logistic regression
data <- list ("n", "n.age", "n.edu", "n.state", "n.region", "y", "female", 
  "black", "age", "edu", "state", "region", "v.prev")

election.inits <- function (){
 list(b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1), 
      mu.age=rnorm(1), mu.edu=rnorm(1), mu.age.edu=rnorm(1), mu.region=rnorm(1),
  b.state=rnorm(n.state), b.v.prev=rnorm(1), sigma.age=runif(1), 
  sigma.edu=runif(1), sigma.age.edu=runif(1), sigma.state=runif(1), sigma.region=runif(1))
}

params <- c ("b.0", "mu.adj", "b.female", "b.black", "b.female.black", "b.age.adj", "b.edu.adj",
   "b.age.edu.adj", "b.region.adj", "b.state",  "b.v.prev", "sigma.age", "sigma.edu",
   "sigma.age.edu", "sigma.state", "sigma.region")
 
election.ch19 <- bugs (data, election.inits, params, "election.ch19.bug", n.chains=3, 
   n.iter=500, bugs.directory="c:/Arquivos de programas/WinBUGS14/",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )

plot (election.ch19)
print (election.ch19)

