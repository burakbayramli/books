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

radon.data <- list ("n", "J", "y", "county", "x")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1), sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

radon.1 <- jags (radon.data, radon.inits, radon.parameters,
  "radon.1.bug", n.chains=3, n.iter=200)

plot (radon.1)
print (radon.1)

attach.jags(radon.1)

print (mean (a[,36] > a[,26]))

a.multilevel <- rep (NA, J)
for (j in 1:J){
  a.multilevel[j] <- median (a[,j])
}
b.multilevel <- median (b)

y.hat <- a.multilevel[county] + b.multilevel*x
y.resid <- y - y.hat

plot (y.hat, y.resid)

