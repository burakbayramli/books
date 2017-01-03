library ("foreign")
library("arm")

iq.data <- read.dta ("~/kod/mcmc/doc/gelman/ARM_Data/child.iq/kidiq.dta")
attach(iq.data)

fit.2 <- lm (kid_score ~ mom_iq)
fit.2.sim <- sim (fit.2)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")

print (fit.2.sim)

for (i in 1:10){
  curve (fit.2.sim$coef[i,1] + fit.2.sim$coef[i,2]*x, add=TRUE,col="gray")
}
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="black")
