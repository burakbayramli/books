## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

## Regression line as a function of one input variable
fit.2 <- lm (kid_score ~ mom_iq)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

 # alternately
curve (cbind(1,x) %*% coef(fit.2), add=TRUE)

### Two fitted regression lines

 ## model with no interaction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

  # alternative sequence of commands
plot(mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
  type="n")
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], pch=20, col="black")
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=20, col="gray")
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

 ## model with interaction
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")

### Displaying uncertainty in the fitted regression (Figure 3.10)
fit.2 <- lm (kid_score ~ mom_iq)
display(fit.2)

fit.2.sim <- sim (fit.2)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score", pch=20)
for (i in 1:10){
  curve (fit.2.sim$coef[i,1] + fit.2.sim$coef[i,2]*x, add=TRUE,col="gray")
}
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="red")

 # alternatively
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score", pch=20)
Oneline <- function (beta) {curve (beta[1]+beta[2]*x, add=TRUE, col="gray")}
apply (fit.2.sim$coef, 1, Oneline)
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="red")


### Displaying using one plot for each input variable (Figure 3.11)
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
beta.hat <- coef (fit.3)
beta.sim <- sim (fit.3)$coef

kidscore.jitter <- jitter(kid_score)

jitter.binary <- function(a, jitt=.05){
   ifelse (a==0, runif (length(a), 0, jitt), runif (length(a), 1-jitt, 1))
}

jitter.mom_hs <- jitter.binary(mom_hs)

par (mfrow=c(1,2))
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score", 
  pch=20, xaxt="n", yaxt="n")
axis (1, c(80,100,120,140))
axis (2, c(20,60,100,140))
for (i in 1:10){
  curve (cbind (1, mean(mom_hs), x) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
}
curve (cbind (1, mean(mom_hs), x) %*% beta.hat, col="black", add=TRUE)
        
plot (jitter.mom_hs, kidscore.jitter, xlab="Mother completed high school",
  ylab="Child test score", pch=20, xaxt="n", yaxt="n")
axis (1, seq(0,1))
axis (2, c(0,50,100,150))
for (i in 1:10){
  curve (cbind (1, x, mean(mom_iq)) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
}
curve (cbind (1, x, mean(mom_iq)) %*% beta.hat, col="black", add=TRUE)


