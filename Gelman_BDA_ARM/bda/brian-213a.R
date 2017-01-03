y <- c(24,25,31,31,22,21,26,20,16,22)
theta=rgamma(1000, sum(y)+0.5, length(y)+0.01)
ypred=rpois(1000, theta)
sort(ypred)[c(25, 975)]
