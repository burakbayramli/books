## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("../../ARM_Data/child.iq/kidiq.dta")
attach(kidiq)

## Fit the model
fit.2 <- lm (kid_score ~ mom_iq)
resid <- fit.2$residuals
sd.resid <- sd(resid)

 # Figure 3.12
plot (mom_iq, resid, xlab="Mother IQ score", ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)

