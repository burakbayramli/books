library ("foreign")
library ("arm")
wells <- read.table ("../doc/gelman/ARM_Data/arsenic/wells.dat", header=TRUE)
attach(wells)

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
print (summary(fit.1))

sim.1 <- sim (fit.1, n.sims=1000)

a <- c(1.,30.)
X.tilde <- matrix(a, nrow = 1, ncol = 2) # example new household

n.sims <- 1000  
n.tilde <- nrow (X.tilde)
print (n.tilde)
y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  p.tilde <- invlogit (X.tilde %*% sim.1$coef[s,])
  y.tilde[s,] <- rbinom (n.tilde, 1, p.tilde)
}

print (y.tilde)
print (sum(y.tilde))
