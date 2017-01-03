library("arm")
library("lme4")

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

## Varying-intercept model w/ no predictors
M0 <- lmer (y ~ 1 + (1 | county))

coef(M0)
fixef (M0)
ranef (M0)
se.fixef(M0)
se.ranef(M0)

M1 <- lmer (y ~ x + (1 | county))
print (summary(M1))

coef(M1)
fixef (M1)
ranef (M1)
se.fixef(M1)
se.ranef(M1)

