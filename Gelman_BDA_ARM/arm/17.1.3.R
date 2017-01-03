library("MCMCpack")
library(R2jags) 

srrs2 <- read.table ("../doc/gelman/ARM_Data/radon/srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"
radon <- srrs2$activity[mn]
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)

for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

X <- cbind (1,x)
K <- ncol (X)

print (K)
quit()

W <- diag (K)
radon.data <- list ("n", "J", "K", "X", "y", "county", "W")

radon.inits <- function (){
  list (B.raw=array(rnorm(J*K), c(J,K)), mu.raw=rnorm(K), sigma.y=runif(1),
       Tau.B.raw=rwish(K+1,diag(K)), xi=runif(K))
}

radon.parameters <- c ("B", "mu", "sigma.y", "sigma.B", "rho.B")

M2 <- jags (radon.data, radon.inits, radon.parameters, "wishart2.bug",
    n.chains=3, n.iter=2000)

print (M2)
