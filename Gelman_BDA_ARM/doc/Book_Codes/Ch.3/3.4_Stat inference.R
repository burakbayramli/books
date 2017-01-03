
## Read the data

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

## Fit the model

fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
display(fit.3)
print(fit.3)
summary(fit.3)

