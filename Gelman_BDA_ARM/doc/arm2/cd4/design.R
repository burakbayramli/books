# Simulation for power calculation for hypothetical new cd4 experiment

# 1.  Function to simulate hypothetical data

cd4.fake <- function (J, K){
  time <- rep (seq(0,1,length=K), J) # K measurements during the year
  person <- rep (1:J, each=K)        # person ID's
  treatment <- sample (rep (0:1, J/2))
  treatment1 <- treatment[person]
#                                    # hyperparameters
  mu.a.true <- 4.8
  g.0.true <- -0.5
  g.1.true <- 0.5
  sigma.y.true <- 0.7
  sigma.a.true <- 1.3
  sigma.b.true <- 0.7
#                                    # person-level parameters
  a.true <- rnorm (J, mu.a.true, sigma.a.true)
  b.true <- rnorm (J, g.0.true + g.1.true*treatment, sigma.b.true)
#                                    # data
  y <- rnorm (J*K, a.true[person] + b.true[person]*time, sigma.y.true)
  return (data.frame (y, time, person, treatment1))
}

# 2.  Function to fit the model and calculate the power

cd4.power <- function (J, K, n.sims=1000){
  signif <- rep (NA, n.sims)
  for (s in 1:n.sims){
    fake <- cd4.fake (J, K)
    lme.power <- lmer (y ~ time + time:treatment1 +
      (1 + time | person), data=fake)
    theta.hat <- fixef(lme.power)["time:treatment1"]
    theta.se <- se.fixef(lme.power)["time:treatment1"]
    signif[s] <- ifelse (theta.hat - 2*theta.se > 0, 1, 0)
  }
  power <- mean (signif)
  return (power)
}

cd4.power (J=150, K=7, n.sims=10)

# 3.  Loop for power calculations

J.values <- c(15, 60, 100, 150, 200, 225, 250, 300, 400)
n.sims.values <- rep(1000,9)
K.values <- c(3,5,7,10)
power.values <- array (NA, c(length(J.values),length(K.values)))
for (i1 in 1:length(J.values)){
  for (i2 in 1:length(K.values)){
    cat ("computing power calculation for J =", J.values[i1], ", K =", K.values[i2], "\n")
    power.values[i1,i2] <- cd4.power (J=J.values[i1], K=K.values[i2], n.sims=n.sims.values[i1])
    cat ("power =", power.values[i1,i2], "\n")
  }
}

# plot all the curves

plot (c(0,max(J.values)), c(0,1), xaxs="i", yaxs="i", xlab="number of children", ylab="power", type="n")
for (i2 in 1:length(K.values)){
  lines (c(0,J.values), c(.025,power.values[,i2]))
}

# just plot the curve for K=7

postscript ("c:/books/multilevel/power.ps", height=3.6, width=4)
plot (c(0,J.values), c(.025,power.values[,3]), ylim=c(0,1), yaxs="i", xaxs="i", type="l",
  xlab="number of children", ylab="power", yaxt="n", mgp=c(1.6,.5,0))
axis (2, seq(0,1,.1),rep("",11), mgp=c(1.6,.5,0), tcl=-.2)
axis (2, seq(0,1,.5),c("0","0.5","1"), mgp=c(1.6,.5,0))
dev.off ()
