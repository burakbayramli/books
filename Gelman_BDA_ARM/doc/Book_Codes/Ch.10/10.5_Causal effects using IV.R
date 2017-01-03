## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sesame
library ("arm")
sesame <- read.dta("sesame.dta")
attach.all (sesame)

## Rename variables of interest

watched <- regular
encouraged <- encour
y <- postlet

## Instrumental variables estimate

fit.1a <- lm (watched ~ encouraged)
display(fit.1a)
 
fit.1b <- lm (y ~ encouraged)
display(fit.1b)

iv.est.1 <- coef (fit.1b)["encouraged"]/coef (fit.1a)["encouraged"]
print(iv.est.1)
 # or

iv.est.2 <- coef (fit.1b)[2]/coef (fit.1a)[2]
print(iv.est.2)
