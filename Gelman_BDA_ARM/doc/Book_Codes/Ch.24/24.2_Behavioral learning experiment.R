## Read data and define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/dogs

y1 <- as.matrix (read.table ("dogs.dat"), nrows=30, ncol=25)
y <- ifelse (y1[,]=="S",1,0)

## Calling predictive replications in Bugs
n.dogs <- nrow(y)
n.trials <- ncol(y)
data <- list("y", "n.dogs", "n.trials")
inits <- function (){
   list (b.0=rnorm(1), b.1=rnorm(1), b.2=rnorm(1))
}
parameters <- c ("b.0", "b.1", "b.2", "y.rep", "y.mean.rep")

fit.logit.1 <- bugs (data, inits, parameters, "dogs.logit.1.bug", n.chains=3,
  n.iter=2000, n.thin=100, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )

## Pedictive replications in R
y.rep <- array (NA, c(n.sims, n.dogs, n.trials))
for (j in 1:n.dogs){
  n.avoid.rep <- rep (0, n.sims)
  n.shock.rep <- rep (0, n.sims)
  for (t in 1:n.trials){  
    p.rep <- invlogit (b.0 + b.1*n.avoid.rep + b.2*n.schok.rep)
    y.rep[,j,t] <- rbinom (n.sims, 1, p.rep)
    n.avoid.rep <- n.avoid.rep + 1 - y.rep[,j,t] 
    n.shock.rep <- n.shock.rep + y.rep[,j,t] 
    }
}

## Direct comparison of simulated to real data
dogsort <- function (y){
  n.dogs <- nrow(y)
  n.trials <- ncol(y)
  last.shock <- rep (NA, n.dogs)
  for (j in 1:n.dogs){
    last.shock[j] <- max ((1:n.trials)[y[j,]==1])
  }
  y[order(last.shock),]
}
   
## More focused model checks

 # Figure 24.2a
test <- function (data){
  colMeans (1-data)
}
plot (c(0, n.trials-1), c(0,1), xlab="Time", ylab="Proportion of avoidances",
  type="n")
mtext ("data and replicated data\n(logit link)", 3)
for (s in 1:20){
  lines (0:(n.trials-1), test (y.sim[s,,]), lwd=.5, col="gray")
}
lines (0:(n.trials-1), test (y), lwd=3)

 # Figure 24.2b
test.diff <- function (data, data.rep){
  test (data) - test (data.rep)
}

diff.range <- NULL
for (s in 1:20){
  diff.range <- range (diff.range, test.diff (y, y.rep[s,,]))
}

plot (c(0, 24), diff.range, xlab="Time", ylab="Proportion of avoidances",
  type="n")
mtext ("data minus replicated data\n(logit link)", 3)
for (s in 1:20){
  lines (0:(n.trials-1), test.diff (y, y.sim[s,,]), lwd=.5, col="gray")
}
abline (0, 0, lwd=3)




