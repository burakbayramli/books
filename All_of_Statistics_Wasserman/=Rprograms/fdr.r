bonferroni.fun <- function(p,alpha){
     m   <- length(p)
     out <- rep(0,m)
     out[p < (alpha/m)] <- 1
     out
     }

fdr.fun <- function(p,alpha){
     m   <- length(p)
     out <- rep(0,m)
     pp  <- sort(p)
     b   <- (1:m)[pp < ((1:m)*alpha/m)]
     if(length(b)>0)thresh <- pp[max(b)] else thresh <- 0
     out[p <= thresh] <- 1
     out
     }


p <- c(runif(5),rbeta(5,1,100))
p <- sort(p)
print(p)
alpha <- .05
m <- length(p)
print(alpha/m)
print(bonferroni.fun(p,alpha))
print(fdr.fun(p,alpha))


