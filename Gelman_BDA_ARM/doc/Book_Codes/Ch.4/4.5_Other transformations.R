## Read the data

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

## Fit the model
fit <- lm (kid_score ~ as.factor(mom_work))
display(fit)

 
