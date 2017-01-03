## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

fit.2 <- lm (kid_score ~ mom_hs + mom_iq)

plot(mom_iq,kid_score, xlab="Mother IQ score", 
  ylab="Child test score",pch=20, xaxt="n", yaxt="n", type="n")
curve (coef(fit.2)[1] + coef(fit.2)[2] + coef(fit.2)[3]*x, add=TRUE, col="gray")
curve (coef(fit.2)[1] + coef(fit.2)[3]*x, add=TRUE)
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=19)
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], col="gray", pch=19)
axis (1, c(80,100,120,140))
axis (2, c(20,60,100,140))




