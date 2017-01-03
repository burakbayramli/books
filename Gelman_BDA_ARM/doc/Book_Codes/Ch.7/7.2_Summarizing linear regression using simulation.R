## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work

source ("4.4_Log transformations.R") # where variables were created 

## Simulation to represent predictive uncertainty

 # Model of log earnings with interactions

earn.logmodel.3 <- lm (log.earn ~ height + male + height:male)
display (earn.logmodel.3)

 # Prediction

x.new <- data.frame (height=68, male=1)
pred.interval <- predict (earn.logmodel.3, x.new, interval="prediction", 
  level=.95)

print (exp (pred.interval))

## Constructing the predictive interval using simulation

pred <- exp (rnorm (1000, 9.95, .88))
pred.original.scale <- rnorm (1000, 9.95, .88)

 # Histograms (Figure 7.2)

par (mfrow=c(2,2))
hist (pred.original.scale, xlab="log(earnings)", main="")
hist (pred, xlab="earnings", main="")

## Why do we need simulation for predictive inferences?

pred.man <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*1 + .007*68*1, .88))
pred.woman <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*0 + .007*68*0, .88))
pred.diff <- pred.man - pred.woman
pred.ratio <- pred.man/pred.woman

## Simulation to represent uncertainty in regression coefficients

n.sims <- 1000
fit.1<- lm (log.earn ~ height + male + height:male)
sim.1 <- sim (fit.1, n.sims)

height.coef <- sim.1$coef[,2]
mean (height.coef)
sd (height.coef)
quantile (height.coef, c(.025, .975))

height.for.men.coef <- sim.1$coef[,2] + sim.1$coef[,4]
quantile (height.for.men.coef, c(.025, .975))

## Inside the sim function

#for (s in 1: n.sims){
#  sigma[s] <- sigma.hat*sqrt((n-k)/rchisq (1, n-k))
#  beta[s] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
#}
#return (list (coef=beta, sigma=sigma))

