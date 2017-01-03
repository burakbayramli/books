## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

library("arm")
kidiq <- read.dta("kidiq.dta")
attach(kidiq)

## Plot Figure 3.1
kidscore.jitter <- jitter(kid_score)

jitter.binary <- function(a, jitt=.05){
   ifelse (a==0, runif (length(a), 0, jitt), runif (length(a), 1-jitt, 1))
}

jitter.mom_hs <- jitter.binary(mom_hs)

fit.0 <- lm (kid_score ~ mom_hs)
display(fit.0)

plot(jitter.mom_hs,kidscore.jitter, xlab="Mother completed high school", 
  ylab="Child test score",pch=20, xaxt="n", yaxt="n")
axis (1, seq(0,1))
axis (2, c(20,60,100,140))
abline (fit.0)

## Plot Figure 3.2
fit.1 <- lm (kid_score ~ mom_iq)

plot(mom_iq,kid_score, xlab="Mother IQ score", 
  ylab="Child test score",pch=20, xaxt="n", yaxt="n")
axis (1, c(80,100,120,140))
axis (2, c(20,60,100,140))
abline (fit.1)

