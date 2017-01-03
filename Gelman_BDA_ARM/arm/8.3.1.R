library ("arm")

y <- scan ("../doc/gelman/ARM_Data/lightspeed/lightspeed.dat", skip=4)
light <- lm (y ~ 1)
print (summary(light))

n.sims <- 1000
sim.light <- sim (light, n.sims)

n <- length (y)
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,] <- rnorm (n, sim.light$coef[s], sim.light$coef[s])
}

par (mfrow=c(5,4))
for (s in 1:20){
  hist (y.rep[s,])
}
