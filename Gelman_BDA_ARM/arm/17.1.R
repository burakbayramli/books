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

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (B=array (rnorm (2*J), c(J,2)), sigma.y=runif(1), sigma.a=runif(1), 
        sigma.b=runif(1), rho=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "sigma.b", "rho")
  
radon.bugs.4a <- jags (radon.data, radon.inits, radon.parameters, "radon.4a.bug", 
             n.chains=3, n.iter=10000)

print (radon.bugs.4a)

