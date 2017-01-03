
library ("foreign")
iq.data <- read.dta ("../doc/gelman/ARM_Data/child.iq/kidiq.dta")
attach(iq.data)

fit.2 <- lm (kid_score ~ mom_hs )
print (fit.2)
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
print (fit.3)
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs*mom_iq)
print (fit.4)
