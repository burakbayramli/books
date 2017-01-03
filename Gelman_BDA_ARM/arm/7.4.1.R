library ("foreign")
library ("arm")
wells <- read.table ("../doc/gelman/ARM_Data/arsenic/wells.dat", header=TRUE)
attach(wells)

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
print (summary(fit.1))

sim.1 <- sim (fit.1, n.sims=1000)
plot (sim.1$coef[,1], sim.1$coef[,2], xlab=expression(beta[0]),
  ylab=expression(beta[1]), pch=20)

  