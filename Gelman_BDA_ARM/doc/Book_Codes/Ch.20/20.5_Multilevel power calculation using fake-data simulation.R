## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/cd4

library("arm")
hiv.data <- read.csv ("allvar.csv")
attach.all (hiv.data)

## Eliminate missing data & just consider the "control" patients (treatmnt==1)
## and with initial age between 1 and 5 years
ok <- treatmnt==1 & !is.na(CD4PCT) & (baseage>1 & baseage<5)
attach.all (hiv.data[ok,])

## Redefining variables
y <- sqrt (CD4PCT)
age.baseline <- baseage        # kid's age (yrs) at the beginning of the study
age.measurement <- visage      # kids age (yrs) at the time of measurement
treatment <- treatmnt
time <- visage - baseage

## Set up new patient id numbers from 1 to J
unique.pid <- unique (newpid)
n <- length (y)
J <- length (unique.pid)
person <- rep (NA, n)
for (j in 1:J){
person[newpid==unique.pid[j]] <- j
}

## Fit the model
M1 <- lmer (y ~ time + (1 + time | person))
display (M1)

## Simulating the hypothetical data
CD4.fake <- function(J, K){
  time <- rep (seq(0,1,length=K), J)  # K measurements during the year
  person <- rep (1:J, each=K)         # person ID's
  treatment <- sample (rep(0:1, J/2))
  treatment1 <- treatment[person] 
#                                     # hyperparameters
  mu.a.true <- 4.8                    # more generally, these could
  g.0.true <- -.5                     # be specified as additional
  g.1.true <- .5                      # arguments to the function
  sigma.y.true <- .7
  sigma.a.true <- 1.3
  sigma.b.true <- .7
#                                     # personal-level parameters
  a.true <- rnorm (J, mu.a.true, sigma.a.true)
  b.true <- rnorm (J, g.0.true + g.1.true*treatment, sigma.b.true)
#                                     # data
  y <- rnorm (J*K, a.true[person] + b.true[person]*time, sigma.y.true)
  return (data.frame (y, time, person, treatment1))
}

## Fitting the model and checking the power
CD4.power <- function (J, K, n.sims=1000){
  signif <- rep (NA, n.sims)
  for (s in 1:n.sims){
    fake <- CD4.fake (J,K)
    lme.power <- lmer (y ~ time + time:treatment1 + (1 + time | person),
         data=fake)
    theta.hat <- fixef(lme.power)["time:treatment1"]
    theta.se <- se.fixef(lme.power)["time:treatment1"]
    signif[s] <- (theta.hat - 2*theta.se) > 0    # return TRUE or FALSE
  }
  power <- mean (signif)                         # proportion of TRUE
  return (power)
}

## Figure 20.5 (a)
for (j in 1:J){
if(j==1){
plot(time[newpid==unique.pid[j]], y[newpid==unique.pid[j]], xlab="time (years)", ylab="sqrt (CD4%)", 
     main="observed data", ylim=c(0,8))
}
points(time[newpid==unique.pid[j]], y[newpid==unique.pid[j]], type="l", ylim=c(0,8))
}

## Figure 20.5 (b)
coef.1 <- matrix(0, J, 1)
coef.2 <- matrix(0, J, 1)
coef.1 <- coef(M1)$person[1]
coef.2 <- coef(M1)$person[2]
for (j in 1:J){
if(j==1){
plot(time[newpid==unique.pid[j]], y[newpid==unique.pid[j]], xlab="time (years)", ylab="sqrt (CD4%)", 
     main="estimated trend lines", xlim=c(0,2), ylim=c(0,8))
}
curve(coef.1[j,1] + coef.2[j,1]*x, add=T)
}
