naive <- function(n){
     x <- rnorm(n)
     sum(x>3)/n
     }

importance <- function(n){
     x <- rnorm(n,4)
     mean(dnorm(x)*(x>3)/dnorm(x,3))
     }

n <- 100
true <- 1-pnorm(3)
est1 <- rep(0,1000)
est2 <- rep(0,1000)
for(i in 1:1000){
     est1[i] <- naive(n)
     est2[i] <- importance(n)
     }

print(true)
print(mean(est1));print(sqrt(var(est1)))
print(mean(est2));print(sqrt(var(est2)))


