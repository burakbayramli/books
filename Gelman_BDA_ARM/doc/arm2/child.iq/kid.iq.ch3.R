## code for nlsy kid iq data for Chapter 3

kidiq <- read.dta(file="kidiq.dta")

attach(kidiq)

### fitting and summarizing regressions in R

fit.3 <- lm (kid.score ~ mom.hs + mom.iq)
display(fit.3)
print(fit.3)
summary(fit.3)

### graphical displays of data and fitted models

fit.2 <- lm (kid.score ~ mom.iq)
plot (mom.iq, kid.score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

# alternately
curve (cbind(1,x) %*% coef(fit.2), add=TRUE)

### two fitted regression lines

## model with no interaction
fit.3 <- lm (kid.score ~ mom.hs + mom.iq)
colors <- ifelse (mom.hs==1, "black", "gray")
plot (mom.iq, kid.score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

# alternative sequence of commands
plot(mom.iq,kid.score,xlab="Mother IQ score",ylab="Child test score",type="n")
points (mom.iq[mom.hs==1], kid.score[mom.hs==1], pch=20, col="black")
points (mom.iq[mom.hs==0], kid.score[mom.hs==0], pch=20, col="gray")
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

### two fitted regression lines: 

## model with interaction
fit.4 <- lm (kid.score ~ mom.hs + mom.iq + mom.hs:mom.iq)
colors <- ifelse (mom.hs==1, "black", "gray")
plot (mom.iq, kid.score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")

### displaying uncertainty in the fitted regression
fit.2 <- lm (kid.score ~ mom.iq)
display(fit.2)

fit.2.sim <- sim (fit.2)
plot (mom.iq, kid.score, xlab="Mother IQ score", ylab="Child test score")
for (i in 1:10){
  curve (fit.2.sim$beta[i,1] + fit.2.sim$beta[i,2]*x, add=TRUE,col="gray")
}
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="black")

# alternatively
Oneline <- function (beta) {curve (beta[1]+beta[2]*x, add=TRUE, col="gray")}
apply (fit.2.sim$beta, 1, Oneline)


### displaying using one plot for each input variable

fit.3 <- lm (kid.score ~ mom.hs + mom.iq)
beta.hat <- coef (fit.3)
beta.sim <- sim (fit.3)$beta

par (mfrow=c(1,2))

plot (mom.iq, kid.score, xlab="Mother IQ score", ylab="Child test score")
for (i in 1:10){
  curve (cbind (1, mean(mom.hs), x) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
}
curve (cbind (1, mean(mom.hs), x) %*% beta.hat, col="black", add=TRUE)
        
plot (mom.hs, kid.score, xlab="Mother completed high school",
  ylab="Child test score")
for (i in 1:10){
  curve (cbind (1, x, mean(mom.iq)) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
}
curve (cbind (1, x, mean(mom.iq)) %*% beta.hat, col="black", add=TRUE)


### Prediction
x.new <- data.frame (mom.hs=1, mom.iq=100)
predict (fit.3, x.new, interval="prediction", level=0.95)


### Exercises
z.scores <- rep (NA, 100)
for (k in 1:100) {
  var1 <- rnorm (1000,0,1)
  var2 <- rnorm (1000,0,1)
  fit <- lm (var2 ~ var1)
  z.scores[k] <- coef(fit)[2]/se.coef(fit)[2]
}

