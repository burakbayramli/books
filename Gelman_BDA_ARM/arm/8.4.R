unemployment <- read.table ("../doc/gelman/ARM_Data/unemployment/unemployment.dat", header=TRUE)
year <- unemployment$year
y <- unemployment$unemployed.pct

n <- length (y)
y.lag <- c (NA, y[1:(n-1)])
lm.lag <- lm (y ~ y.lag)

print (y)
print (y.lag)

print (summary(lm.lag))
print (lm.lag)

b.hat <- coef (lm.lag)
s.hat <- summary(lm.lag)$sigma

print (b.hat)
print (s.hat)

