## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/arsenic

library("arm")
wells <- read.table ("wells.dat")
attach.all (wells)

## Logistic regression 

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.1)

 # Figure 7.6 (a)

sim.1 <- sim (fit.1, n.sims=1000)
plot (sim.1$coef[,1], sim.1$coef[,2], xlab=expression(beta[0]),
  ylab=expression(beta[1]), pch=20)

 # Figure 7.6 (b) 

plot (dist, switch, xlab="Distance (in meters) to the nearest safe well",
  ylab="Pr(switching)")
for (s in 1:20){
  curve (invlogit (sim.1$coef[s,1] + sim.1$coef[s,2]*x), col="grey", 
  add=TRUE)
}
curve (invlogit (fit.1$coef[1] + fit.1$coef[2]*x), add=TRUE, lwd=2)

## Predictive simulation using the binomial distribution

n.sims <- 1000
X.tilde <- cbind (1, dist)
n.tilde <- nrow (X.tilde)
y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  p.tilde <- invlogit (X.tilde %*% sim.1$coef[s,])
  y.tilde <- rbinom (n.tilde, 1, p.tilde)
}

## Predictive simulation using latent logistic distribution

logit <- function (a) {log(a/(1-a))}

y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  epsilon.tilde <- logit (runif (n.tilde, 0, 1))
  z.tilde <- X.tilde %*% sim.1$coef[s,] + epsilon.tilde
  y.tilde[s,] <- ifelse (z.tilde>0, 1, 0)
}

# Alternative using matrix algebra

epsilon.tilde <- array (logit (runif (n.sims*n.tilde, 0, 1)),
                        c(n.sims, n.tilde))
z.tilde <- sim.1$coef %*% t(X.tilde) + epsilon.tilde
y.tilde <- ifelse (z.tilde>0, 1, 0)

################################################################################
## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work

source ("6.7_More complex glm.R") # where variables were created

### Compound models

 ## Models

fit.1a <- glm (earn.pos ~ height + male, family=binomial(link="logit"))
display (fit.1a)

log.earn <- log (earn)
fit.1b <- lm (log.earn ~ height + male, subset=earn>0)
display (fit.1b)

x.new <- c (1, 68, 1)          # constant term=1, height=68, male=1

 # Simulation ignoring uncertainty

n.sims <- 1000
prob.earn.pos <- invlogit (coef(fit.1a) %*% x.new)
earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)
earn.sim <- ifelse (earn.pos.sim==0, 0, 
  exp (rnorm (n.sims, coef(fit.1b) %*% x.new, sigma.hat(fit.1b))))

 # Simulated values of coefficient estimates

sim.1a <- sim (fit.1a, n.sims)
sim.1b <- sim (fit.1b, n.sims)
prob.earn.pos <- invlogit (sim.1a$coef %*% x.new)
earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)
earn.sim <- ifelse (earn.pos.sim==0, 0,
  exp (rnorm (n.sims, sim.1b$coef %*% x.new, sim.1b$sigma)))

  # Computations into a function

Mean.earn <- function (height, male, sim.a, sim.b){
  x.new <- c (1, height, male)
  prob.earn.pos <- invlogit (sim.a$coef %*% x.new)
  earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)  
  earn.sim <- ifelse (earn.pos.sim==0, 0,
    exp (rnorm (n.sims, sim.b$coef %*% x.new, sim.b$sigma)))
  return (mean (earn.sim))
}

heights <- seq (60, 75, 1)
mean.earn.female <- sapply (heights, Mean.earn, male=0, sim.1a, sim.1b)
mean.earn.male <- sapply (heights, Mean.earn, male=1, sim.1a, sim.1b)

  # or

heights <- seq (60, 75, 1)
k <- length (heights) 
mean.earn.female <- rep (NA, k)
mean.earn.male <- rep (NA, k)
for (i in 1:k){
  mean.earn.female[i] <- Mean.earn (heights[i], 0, sim.1a, sim.1b)
  mean.earn.male[i] <- Mean.earn (heights[i], 1, sim.1a, sim.1b)
}

plot (heights, mean.earn.female, type="l")
