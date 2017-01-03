# Model:  y = 4+ 2x1 + 3x2 - 5x3 + N(0,1)

n <- 15  # sample size

x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)

beta <- c(4, 2, 3, -5)
y <- 4+ 2*x1 + 3*x2 - 5*x3 + rnorm(n)
y.lm <- lm(y ~ x1 + x2 + x3)
X <- model.matrix(y.lm)
round(t(X)%*%X,2)

betahat <- coef(summary(y.lm))[,1]
sebeta <- coef(summary(y.lm))[,2]
upper <- betahat + qt(1-.05/2, n-4)*sebeta
lower <- betahat - qt(1-.05/2, n-4)*sebeta
bonupper <- betahat + qt(1-.05/(2*4), n-4)*sebeta
bonlower <- betahat - qt(1-.05/(2*4), n-4)*sebeta
scheffeupper <- betahat + sqrt(2*qf(1-.05, 4, n-4))*sebeta
scheffelower <- betahat - sqrt(2*qf(1-.05, 4, n-4))*sebeta
round(cbind(lower, upper, bonlower, bonupper, 
  scheffelower, scheffeupper, beta), 2)

all((lower < beta)&(upper > beta))
all((bonlower < beta)&(bonupper > beta))
all((scheffelower < beta)&(scheffeupper > beta))

cicorrect1 <- NULL
cicorrect2 <- NULL
cicorrect3 <- NULL
cicorrect4 <- NULL
cicorrect <- NULL
boncorrect <- NULL
scheffecor <- NULL
for (i in 1:10000) {
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
beta <- c(4, 2, 3, -5)
y <- 4 + 2*x1 + 3*x2 - 5*x3 + rnorm(n)
y.lm <- lm(y ~ x1 + x2 + x3)
betahat <- coef(summary(y.lm))[,1]
sebeta <- coef(summary(y.lm))[,2]
upper <- betahat + qt(1-.05/2, n-4)*sebeta
lower <- betahat - qt(1-.05/2, n-4)*sebeta
bonupper <- betahat + qt(1-.05/(2*4), n-4)*sebeta
bonlower <- betahat - qt(1-.05/(2*4), n-4)*sebeta
scheffeupper <- betahat + sqrt(2*qf(1-.05, 4, n-4))*sebeta
scheffelower <- betahat - sqrt(2*qf(1-.05, 4, n-4))*sebeta
round(cbind(lower, upper, bonlower, bonupper,
  scheffelower, scheffeupper, beta), 2)
cicorrect1 <- c(cicorrect1,((lower[1] < beta[1])&(upper[1] > beta[1])))
cicorrect2 <- c(cicorrect2,((lower[2] < beta[2])&(upper[2] > beta[2])))
cicorrect3 <- c(cicorrect3,((lower[3] < beta[3])&(upper[3] > beta[3])))
cicorrect4 <- c(cicorrect4,((lower[4] < beta[4])&(upper[4] > beta[4])))
cicorrect <- c(cicorrect,all((lower < beta)&(upper > beta)))
boncorrect <- c(boncorrect, all((bonlower < beta)&(bonupper > beta)))
scheffecor <- c(scheffecor, all((scheffelower < beta)&(scheffeupper > 
beta)))
}
sapply(data.frame(cicorrect, boncorrect, scheffecor), mean)
sapply(data.frame(cicorrect1, cicorrect2, cicorrect3, cicorrect4), mean)

