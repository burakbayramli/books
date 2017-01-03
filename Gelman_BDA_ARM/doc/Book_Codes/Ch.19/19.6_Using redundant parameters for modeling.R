## Read, clean the pilots data, redefine variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/schools

library ("arm")
schools <- read.table ("schools.dat", header=TRUE)
attach.all (schools)

J <- 8
y <- c(28,8,-3,7,-1,1,18,12)
sigma.y <- c(15,10,16,11,9,11,10,18)
schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
schools.parameters <- c("theta", "mu.theta", "sigma.theta")

 # call bugs
schools.sim <- bugs (schools.data, schools.inits, schools.parameters,
  "schools.bug", n.chains=3, n.iter=1000, bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, debug=TRUE )

 # display the results on console and graphically
print (schools.sim)
plot (schools.sim)
