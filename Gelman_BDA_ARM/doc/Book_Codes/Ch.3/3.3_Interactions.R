## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

fit <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
display (fit)

## Figure 3.4 (a)
plot(mom_iq,kid_score, xlab="Mother IQ score", 
  ylab="Child test score",pch=20, xaxt="n", yaxt="n", type="n")
curve (coef(fit)[1] + coef(fit)[2] + (coef(fit)[3] + coef(fit)[4])*x, add=TRUE, col="gray")
curve (coef(fit)[1] + coef(fit)[3]*x, add=TRUE)
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=20)
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], col="gray", pch=20)
axis (1, c(80,100,120,140))
axis (2, c(20,60,100,140))

## Figure 3.4 (b)

plot(mom_iq,kid_score, xlab="Mother IQ score", 
  ylab="Child test score",pch=20, type="n", xlim=c(0,150), ylim=c(0,150))
curve (coef(fit)[1] + coef(fit)[2] + (coef(fit)[3] + coef(fit)[4])*x, add=TRUE, col="gray")
curve (coef(fit)[1] + coef(fit)[3]*x, add=TRUE)
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=20)
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], col="gray", pch=20)



