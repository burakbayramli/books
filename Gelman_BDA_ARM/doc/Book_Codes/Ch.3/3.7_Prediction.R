## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

## Model fit and prediction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
x.new <- data.frame (mom_hs=1, mom_iq=100)
predict (fit.3, x.new, interval="prediction", level=0.95)

## Figure 3.13
 # Note: different data! Available at "Data for figure 3.13" file
kid.iq <- read.table("kid.iq.txt", header=T)  

 # normalize mom iq to have mean of 100 and sd of 15 & denote it by afqt
kid.iq$afqt <- (kid.iq$afqt.adj-mean(kid.iq$afqt.adj))*(15/sqrt(var(kid.iq$afqt.adj))) + 100
kid.iq$hs <- as.numeric(kid.iq$educ.cat!=1)

 # external validation
kid.iq.ev <- read.table("kid.iq.ext.val.txt", header=T) # Note: different data! Available at "Data for figure 3.13" file
kid.iq.ev$afqt <- (kid.iq.ev$afqt.adj-mean(kid.iq$afqt.adj))*(15/sqrt(var(kid.iq.ev$afqt.adj))) + 100
kid.iq.ev$hs <- as.numeric(kid.iq.ev$educ.cat!=1)

 # fit the model and make the plots
fit.all <- lm (ppvt ~ hs + afqt, data=kid.iq)
display(fit.all)

cscores.new <- fit.all$coef[1] + fit.all$coef[2]*kid.iq.ev$hs + fit.all$coef[3]*kid.iq.ev$afqt
res <- (kid.iq.ev$ppvt - cscores.new)
a <- sqrt(var(res))

par(mfrow=c(2,2))
plot(cscores.new,kid.iq.ev$ppvt, xlab="Predicted score", ylab="Actual score",col="darkgrey", mgp=c(2,.5,0), pch=20, cex=1, xlim=c(20,140), ylim=c(20,140))
curve(1*x, add=T)

plot(cscores.new, res,xlab="Predicted score", ylab="Prediction error", col="darkgrey", mgp=c(2,.5,0), pch=20,cex=1)
abline(h=0)
abline(h=a, lty=2)
abline(h=-a, lty=2)