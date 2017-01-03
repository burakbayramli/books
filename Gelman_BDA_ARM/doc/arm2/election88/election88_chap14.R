# Fitting models for chapter 13

attach.all (polls.subset)

# 1.  Fit simple multilevel logistic regression in lmer

M1 <- lmer (y ~ black + female + (1 | state), family=binomial(link="logit"))
display (M1)

# 1.  Fit multilevel logistic regression in lmer

age.edu <- n.edu*(age-1) + edu
region.full <- region[state]
v.prev.full <- v.prev[state]

M2 <- lmer (y ~ black + female + black:female + v.prev.full + (1 | age) + (1 | edu) + (1 | age.edu) + (1 | state) + (1 | region.full), family=binomial(link="logit"))
display (M2)

# 2.  Fit the model in Bugs

data <- list ("n", "n.age", "n.edu", "n.state", "n.region",
 "y", "female", "black", "age", "edu", "state", "region", "v.prev")
inits <- function () {list(
  b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
  a.age=rnorm(n.age), a.edu=rnorm(n.edu),
  a.age.edu=array (rnorm(n.age*n.edu), c(n.age,n.edu)),
  a.state=rnorm(n.state), a.region=rnorm(n.region),
  sigma.age=runif(1), sigma.edu=runif(1), sigma.age.edu=runif(1),
  sigma.state=runif(1), sigma.region=runif(1))
}
params <- c ("b.0", "b.female", "b.black", "b.female.black",
   "a.age", "a.edu", "a.age.edu", "a.state", "a.region",
   "sigma.age", "sigma.edu", "sigma.age.edu", "sigma.state", "sigma.region")
M2.bugs <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=1000)

# Fit using openbugs

M2.bugs.a <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=10)
M2.bugs.b <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=10, program="OpenBUGS")
M2.bugs.c <- openbugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=10)

# 2.  Fit the model in Bugs with redundant parameterization

data <- list ("n", "n.age", "n.edu", "n.state", "n.region",
 "y", "female", "black", "age", "edu", "state", "region", "v.prev")
inits <- function () {list(
  b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
  a.age.raw=rnorm(n.age), a.edu.raw=rnorm(n.edu),
  a.age.edu.raw=array (rnorm(n.age*n.edu), c(n.age,n.edu)),
  a.state.raw=rnorm(n.state), a.region.raw=rnorm(n.region),
  sigma.age.raw=runif(1), sigma.edu.raw=runif(1), sigma.age.edu.raw=runif(1),
  sigma.state.raw=runif(1), sigma.region.raw=runif(1),
  xi.age=runif(1), xi.edu=runif(1), xi.age.edu=runif(1), xi.state=runif(1))}
params <- c ("b.0", "b.female", "b.black", "b.female.black",
   "a.age", "a.edu", "a.age.edu", "a.state", "a.region",
   "sigma.age", "sigma.edu", "sigma.age.edu", "sigma.state", "sigma.region")
M3.bugs <- bugs (data, inits, params, "election88.M3.bug", n.chains=3, n.iter=1000)

# 3.  Postprocessing to get state averages

# create linear predictors

attach.bugs (M2.bugs)
linpred <- rep (NA, n)
for (i in 1:n){
  linpred[i] <- mean (b.0 + b.female*female[i] + b.black*black[i] +
    b.female.black*female[i]*black[i] + a.age[,age[i]] + a.edu[,edu[i]] +
    a.age.edu[,age[i],edu[i]])
}

# plot the 8 states

par (mfrow=c(2,4))
y.jitter <- y + ifelse (y==0, runif (n, 0, .1), runif (n, -.1, 0))
state.name.all <- c(state.name[1:8], "District of Columbia", state.name[9:50])
for (j in c(2,3,4,8,6,7,5,9)) {
  plot (0, 0, xlim=range(linpred), ylim=c(0,1), yaxs="i",
        xlab="linear predictor", ylab="Pr (support Bush)",
        main=state.name.all[j], type="n")
  for (s in 1:20){
    curve (invlogit (a.state[s,j] + x), lwd=.5, add=TRUE, col="gray20")}
  curve (invlogit (median (a.state[,j]) + x), lwd=2, add=TRUE)
  if (sum(state==j)>0) points (linpred[state==j], y.jitter[state==j])
}

# create predicted values for each of 3264 strata

L <- nrow (census)
y.pred <- array (NA, c(n.sims, L))
for (l in 1:L){
  y.pred[,l] <- invlogit(b.0 + b.female*census$female[l] +
    b.black*census$black[l] + b.female.black*census$female[l]*census$black[l] +
    a.age[,census$age[l]] + a.edu[,census$edu[l]] +
    a.age.edu[,census$age[l],census$edu[l]] + a.state[,census$state[l]])
}

# average over strata within each state

y.pred.state <- array (NA, c(n.sims, n.state))
for (s in 1:n.sims){
  for (j in 1:n.state){
    ok <- census$state==j
    y.pred.state[s,j] <- sum(census$N[ok]*y.pred[s,ok])/sum(census$N[ok])
  }
}

state.pred <- array (NA, c(n.state,3))
for (j in 1:n.state){
  state.pred[j,] <- quantile (y.pred.state[,j], c(.25,.5,.75))
}
