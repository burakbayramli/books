roaches <- read.csv ("../doc/gelman/ARM_Data/roaches/roachdata.csv")
attach(roaches)

glm.1 <- glm (y ~ roach1 + treatment + senior, family=poisson, offset=log(exposure2))

print (summary(glm.1))

n <- length (y)
X <- cbind (rep(1,n), roach1, treatment, senior)
y.hat <- exposure2 * exp (X %*% coef (glm.1))
y.rep <- rpois (n, y.hat)

print (mean (y==0))
print (mean (y.rep==0))

