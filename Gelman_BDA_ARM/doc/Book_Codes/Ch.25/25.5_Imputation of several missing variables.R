## Read the data
## Read the the Social Indicators Survey data 
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sis

# The R codes & data files should be saved in the same directory for
# the source command to work

source("25.4_Random imputation of a single variable.R") # where variables were defined

## Variables description:

 # interest:  interest of entire family
interest <- na.fix(interest)

 # transforming the different sources of income
interest <- interest/1000

## Simple random imputation
interest.imp <- random.imp (interest)

## Iterative regression imputation
impute <- function (a, a.impute){
   ifelse (is.na(a), a.impute, a)
}

n.sims <- 10
for (s in 1:n.sims){
  lm.1 <- lm (earnings ~ interest.imp + male + over65 + white + immig +
    educ_r + workmos + workhrs.top + any.ssi + any.welfare + any.charity)
  pred.1 <- rnorm (n, predict(lm.1), sigma.hat(lm.1))
  earnings.imp <- impute (earnings, pred.1)

  lm.2 <- lm (interest ~ earnings.imp + male + over65 + white + immig + 
    educ_r + workmos + workhrs.top + any.ssi + any.welfare + any.charity)
  pred.2 <- rnorm (n, predict(lm.2), sigma.hat(lm.2))
  interest.imp <- impute (interest, pred.2)
}