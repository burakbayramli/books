## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sesame

# The R codes & data files should be saved in the same directory for
# the source command to work

source("10.5_Causal effects using IV.R") # where data was cleaned

## Rename variables of interest

pretest <- prelet

## 2 stage least squares

fit.2a <- lm (watched ~ encouraged)
display(fit.2a)
watched.hat <- fit.2a$fitted
fit.2b <- lm (y ~ watched.hat)
display(fit.2b)
 
## Adjusting for covariates in a IV framework

fit.3a <- lm (watched ~ encouraged + pretest + as.factor(site) +
    setting)
display(fit.3a)
watched.hat <- fit.3a$fitted
fit.3b <- lm (y ~ watched.hat + pretest + as.factor(site) +
    setting)
display(fit.3b)
 
## Se for IV estimates

fit.3b <- lm (y ~ watched.hat + pretest + as.factor(site) +
    setting, x=TRUE)
display(fit.3b)

X.adj <- fit.3b$x
X.adj[, "watched.hat"] <- watched
residual.sd.adj <- sd (y - X.adj %*% coef (fit.3b))
se.adj <- se.coef(fit.3b)["watched.hat"]*residual.sd.adj/sigma.hat(fit.3b)

## Performing 2sls automatically

library ("sem")

 # regression without pre-treatment variables

iv1 <- tsls (y ~ watched, instruments= ~ encouraged, data=sesame)
summary (iv1) 

 # regression controlling for pre-treatment variables

iv2 <- tsls (y ~ watched + pretest + as.factor(site) + setting,
   instruments= ~ encouraged + pretest + as.factor(site) + setting,
   data=sesame)
summary (iv2) 




