## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

## Estimations

 # original model
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
display(fit.4)

 # centering by subtracting the mean
c_mom_hs <- mom_hs - mean(mom_hs)
c_mom_iq <- mom_iq - mean(mom_iq)

fit.5 <- lm (kid_score ~ c_mom_hs + c_mom_iq + c_mom_hs:c_mom_iq)
display(fit.5)

 # using a conventional centering point
c2_mom_hs <- mom_hs - 0.5
c2_mom_iq <- mom_iq - 100

fit.6 <- lm (kid_score ~ c2_mom_hs + c2_mom_iq + c2_mom_hs:c2_mom_iq)
display(fit.6)

 # centering by subtracting the mean & dividing by 2 sd
z_mom_hs <- (mom_hs - mean(mom_hs))/(2*sd(mom_hs))
z_mom_iq <- (mom_iq - mean(mom_iq))/(2*sd(mom_iq))

fit.7 <- lm (kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq)
display(fit.7)




