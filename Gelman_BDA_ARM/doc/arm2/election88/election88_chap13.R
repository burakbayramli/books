# Fitting models for chapter 13

attach.all (polls.subset)

# 1.  Fit multilevel logistic regression in lmer

M1 <- lmer (y ~ black*female + (1 | state), family=binomial(link="logit"))
display (M1)

M2 <- lmer (y ~ black*female + (1 | state) + (1 | age) + (1 | edu), family=binomial(link="logit"))
display (M2)

age.edu <- n.edu*(age - 1) + edu
region.full <- region[state]

M3 <- lmer (y ~ black*female + (1 | state) + (1 | age) + (1 | edu) + (1 | age.edu), family=binomial(link="logit"))
display (M3)

# simple test version!


# set up the predictors

n.age <- 4
n.edu <- 4
n.rep <- 100
n.state <- 50
n <- n.age*n.edu*n.rep
age.id <- rep (1:n.age, each=n.edu*n.rep)
edu.id <- rep (1:n.edu, n.age, each=n.rep)
age.edu.id <- n.edu*(age.id - 1) + edu.id
state.id <- sample (1:n.state, n, replace=TRUE)

# simulate the varying parameters

a.age <- rnorm (n.age, 1, 2)
a.edu <- rnorm (n.edu, 3, 4)
a.age.edu <- rnorm (n.age*n.edu, 0, 5)
a.state <- rnorm (n.state, 0, 6)

# simulate the data and print to check that i did it right

y.hat <- a.age[age.id] + a.edu[edu.id] + a.age.edu[age.edu.id] + a.state[state.id]
y <- rnorm (n, y.hat, 1)
print (cbind (age.id, edu.id, age.edu.id, state.id, y.hat, y))

# this model (and simpler versions) work fine:

fit.1 <- lmer (y ~ 1 + (1 | age.id) + (1 | edu.id) + (1 | age.edu.id) + (1 | state.id))

# now go to logistic regression

ypos <- ifelse (y > mean(y), 1, 0)

# these work fine:

fit.2 <- lmer (ypos ~ 1 + (1 | age.id) + (1 | edu.id) + (1 | age.edu.id), family=binomial(link="logit"))
fit.3 <- lmer (ypos ~ 1 + (1 | age.id) + (1 | edu.id) + (1 | state.id), family=binomial(link="logit"))

# this one causes R to crash!!!!!!!

fit.4 <- lmer (ypos ~ 1 + (1 | age.id) + (1 | edu.id) + (1 | age.edu.id) + (1 | state.id), family=binomial(link="logit"))

