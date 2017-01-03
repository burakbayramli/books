library ("arm")
library("R2WinBUGS")
#library(R2jags) 

heightweight <- read.fwf ("../doc/gelman/ARM_Data/earnings/wfw90.dat", widths=c(143,1,2,3))
height1 <- heightweight[,2]
height2 <- heightweight[,3]
weight <- heightweight[,4]
ok <- !is.na (height1+height2+weight) & weight<998 & height1<7 & height2<12
height <- 12*height1[ok] + height2[ok]
weight <- weight[ok]
n <- length(height)

C <- 200

censored <- weight>=C
weight.censored <- ifelse (censored,C,weight)

## Regression
c.height <- height - mean(height)

z <- ifelse (weight.censored==C, NA, weight.censored)

data <- list (x=c.height, y=weight.censored, z=z, n=n, C=C)
inits <- function(){list (a=rnorm(1), b=rnorm(1), sigma.y=runif(1))}
params <- c ("a", "b", "sigma.y")

#censoring.1 <- jags (data, inits, params, "censoring-jags.bug", n.iter=10)
censoring.1 <- bugs (data, inits, params, "censoring.bug", n.iter=10)
print(censoring.1)

