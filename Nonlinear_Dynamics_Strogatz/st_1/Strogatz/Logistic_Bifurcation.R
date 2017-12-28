logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: Number of iteration
  ## M: Number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations
  z[c((N-M):N)]
}

## Set scanning range for bifurcation parameter r
M=300
my.r <- seq(2.5, 4, by=0.005)
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.001, N=1000, M=M))
##   user  system elapsed (on a 2.4GHz Core2Duo)
##  1.834   0.011   1.840

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, (M+1)))

plot(Orbit ~ r, pch=".")
