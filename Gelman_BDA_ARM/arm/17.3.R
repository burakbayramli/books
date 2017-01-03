library(R2jags) 

pilots <- read.table ("../doc/gelman/ARM_Data/pilots/pilots.dat", header=TRUE)
attach(pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- group==group.names[j] & scenario==scenario.names[k]    
    successes <- c (successes, sum(recovered[ok]==1,na.rm=T))
    failures <- c (failures, sum(recovered[ok]==0,na.rm=T))
    group.id <- c (group.id, j)
    scenario.id <- c (scenario.id, k)
  }
}

y <- successes/(successes+failures)
y.mat <- matrix (y, n.scenario, n.group)
sort.group <- order(apply(y.mat,2,mean))
sort.scenario <- order(apply(y.mat,1,mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario,sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Fit the 2-model using Bugs

n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

print (y)
print (treatment)
print (airport)

data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), sigma.delta=runif(1), sigma.gamma=runif(1), sigma.y=runif(1))
}
parameters <- c("mu", "sigma.y", "sigma.gamma", "sigma.delta", "gamma", "delta")

pilots <- jags (data, inits, parameters, "pilots.bug", n.chains=3, n.iter=1000)
print (pilots)

