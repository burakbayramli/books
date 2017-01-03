# Some code from chapter 20

CD4.fake <- function (J, K){
  time <- rep (seq(0,1,length=K), J)   # K measurements during the year
  person <- rep (1:J, each=K)          # person ID's
  treatment <- sample (rep (0:1, J/2))
  treatment1 <- treatment[person]
#                                      # hyperparameters:
  mu.a.true <- 4.8                     #   more generally, these could
  g.0.true <-  -.5                     #   be specified as additional
  g.1.true <-   .5                     #   arguments to the function
  sigma.y.true <-  .7
  sigma.a.true <- 1.3
  sigma.b.true <-  .7
#                                      # person-level parameters
  a.true <- rnorm (J, mu.a.true, sigma.a.true)
  b.true <- rnorm (J, g.0.true + g.1.true*treatment, sigma.b.true)
#                                      # data
  y <- rnorm (J*K, a.true[person] + b.true[person]*time, sigma.y.true)
  return (data.frame (y, time, person, treatment1))
}

CD4.power <- function (J, K, n.sims=1000){
  signif <- rep (NA, n.sims)
  for (s in 1:n.sims){
    fake <- CD4.fake (J, K)
    lme.power <- lmer (y ~ time + time:treatment1 +
      (1 + time | person), data=fake)
    theta.hat <- fixef(lme.power)["time:treatment1"]
    theta.se <- se.fixef(lme.power)["time:treatment1"]
    signif[s] <- (theta.hat - 2*theta.se) > 0     # returns TRUE or FALSE
  }
  power <- mean (signif)                          # proportion of TRUE
  return (power)
}

