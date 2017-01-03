## Read the data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88

# Set up the data for the election88 example

# Load in data for region indicators
# Use "state", an R data file (type ?state from the R command window for info)
#
# Regions:  1=northeast, 2=south, 3=north central, 4=west, 5=d.c.
# We have to insert d.c. (it is the 9th "state" in alphabetical order)

library ("arm")
data (state)                  # "state" is an R data file
state.abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
dc <- 9
not.dc <- c(1:8,10:51)
region <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

# Load in data from the CBS polls in 1988
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88
library (foreign)
polls <- read.dta ("polls.dta")
attach.all (polls)

# Select just the data from the last survey (#9158)
table (survey)                # look at the survey id's
ok <- survey==9158            # define the condition
polls.subset <- polls[ok,]    # select the subset of interest
attach.all (polls.subset)     # attach the subset
write.table (polls.subset, "polls.subset.dat")

print (polls.subset[1:5,])

# define other data summaries
y <- bush                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions

# compute unweighted and weighted averages for the U.S.
ok <- !is.na(y)                                    # remove the undecideds
cat ("national mean of raw data:", round (mean(y[ok]==1), 3), "\n")
cat ("national weighted mean of raw data:",
     round (sum((weight*y)[ok])/sum(weight[ok]), 3), "\n")

# compute weighted averages for the states
raw.weighted <- rep (NA, n.state)
names (raw.weighted) <- state.abbr
for (i in 1:n.state){
  ok <- !is.na(y) & state==i
  raw.weighted[i] <- sum ((weight*y)[ok])/sum(weight[ok])
}

# load in 1988 election data as a validation check
election88 <- read.dta ("election88.dta")
outcome <- election88$electionresult

# load in 1988 census data
census <- read.dta ("census88.dta")

# also include a measure of previous vote as a state-level predictor
presvote <- read.dta ("presvote.dta")
attach (presvote)
v.prev <- presvote$g76_84pr
not.dc <- c(1:8,10:51)
candidate.effects <- read.table ("candidate_effects.dat", header=T)
v.prev[not.dc] <- v.prev[not.dc] +
 (candidate.effects$X76 + candidate.effects$X80 + candidate.effects$X84)/3
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88

## Multilevel logistic regression
M1 <- lmer (y ~ black + female + (1 | state), family=binomial(link="logit"))
display (M1)
 
## A fuller model

 # set up the predictors
age.edu <- n.edu*(age-1) + edu
region.full <- region[state]
v.prev.full <- v.prev[state]

 # fit the model
M2 <- lmer (y ~ black + female + black:female + v.prev.full + (1 | age) + 
  (1 | edu) + (1 | age.edu) + (1 | state) + (1 | region.full), family=binomial(link="logit"))
display (M2)

### Fit the model in Bugs
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
 
M2.bugs <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=1000,
        bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )

## Plot Figure 14.1 
attach.bugs (M2.bugs)
par (mar=c(0,0,0,0))
summ <- M2.bugs$summary[c(2:28),3:7]
labels <- c("female","black","female x black",
            "18-29","30-44","45-64","65+",
            "no h.s.","high school","some college","college grad",
            "18-29 x no h.s.","18-29 x high school","18-29 x some college","18-29 x college grad",
            "30-44 x no h.s.","30-44 x high school","30-44 x some college","30-44 x college grad",
            "45-64 x no h.s.","45-64 x high school","45-64 x some college","45-64 x college grad",
            "65+ x no h.s.","65+ x high school","65+ x some college","65+ x college grad")

pos <- c (1:3, 5:8, 10:13, 15:18, 20:23, 25:28, 30:33)
bottom <- max(pos)+1

rng <- range(summ)
p.rng <- pretty(rng)
a <- -min(p.rng)/(max(p.rng)-min(p.rng))
b <- 1/(max(p.rng)-min(p.rng))
summ.adj <- a + b*summ
plot (c(-.25,1), c(2,-bottom-2), xlab="", ylab="", xaxt="n", yaxt="n",
      type="n", bty="n")
for (i in 1:nrow(summ)){
  text (-.25, -pos[i], labels[i], adj=0, cex=1.1)
  points (summ.adj[i,3], -pos[i], pch=20, cex=1.5)
  lines (summ.adj[i,c(2,4)], rep(-pos[i],2), lwd=4)
  lines (summ.adj[i,c(1,5)], rep(-pos[i],2), lwd=.5)
}
lines (rep(a,2), c(0,-bottom), lwd=.5)
lines (c(0,1), rep(0,2))
lines (c(0,1), rep(-bottom,2))
for (x in p.rng){
  text (a+b*x, 1, x, cex=1.2)
  lines (rep(a+b*x,2), c(0,-.2))
  text (a+b*x, -bottom-1, x, cex=1.2)
  lines (rep(a+b*x,2), -bottom+c(0,.2))
}

## Plot Figure 14.2 

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
  plot (0, 0, xlim=range(linpred), ylim=c(0,1), yaxs="i", pch=20,
        xlab="linear predictor", ylab="Pr (support Bush)",
        main=state.name.all[j], type="n")
  for (s in 1:20){
    curve (invlogit (a.state[s,j] + x), lwd=.5, add=TRUE, col="gray20")}
  curve (invlogit (median (a.state[,j]) + x), lwd=2, add=TRUE)
  if (sum(state==j)>0) points (linpred[state==j], y.jitter[state==j])
}

## Using the model inferences to estimate avg opinion for each state

 # construct the n.sims x 3264 matrix
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

 # average over strata within each state
state.pred <- array (NA, c(n.state,3))
for (j in 1:n.state){
  state.pred[j,] <- quantile (y.pred.state[,j], c(.25,.5,.75))
}

## Plot Figure 14.3
attach (M2.bugs$sims.list)  ## ???
region.name <- c("Northeast", "Midwest", "South", "West", "D.C.")
par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (k in 1:4){
  plot (range(v.prev[not.dc]), range(M2.bugs$median$a.state[not.dc]), cex.lab=1.2,
        cex.axis=1.2, cex.main=1.5, ylim=c(-.7,.7), xaxt="n", yaxt="n",
        xlab="R vote in prev elections",
        ylab="regression intercept", pch=20,
        main=region.name[k], type="n")
  axis (1, c(.5,.6,.7), cex.axis=1.2)
  axis (2, c(-.5,0,.5), cex.axis=1.2)
  for (j in (1:n.state)[region==k]){
    lines (rep(v.prev[j],2), quantile(a.state[,j], c(.25,.75)), lwd=.5, col="gray")
    text (v.prev[j], M2.bugs$median$a.state[j], state.abbr[j], cex=1.2)
  }
  curve (median(a.region[,k]) - .7 + median(v.prev)*x, lwd=.5, add=T)
}

## Plot Figure 14.5


