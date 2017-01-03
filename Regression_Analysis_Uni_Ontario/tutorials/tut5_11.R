## Regression Tutorial V

## Q2
X <- matrix(c(-1,-1,0,0,1,1,0,0,-1,-1,1,1,-1,1,0,0,-1,1), ncol=3, nrow=6)
y <- 1:6
my.lm <- lm(y ~ X-1)
X
y

## b
t(X)%*%y
t(X)%*%X
beta <- solve(t(X)%*%X,t(X)%*%y)
beta

## e 
X %*% beta

## f
res <- y - X %*% beta
res

## g
summary(my.lm)$sigma
sum(res^2)
sig.hat <- t(y) %*% (diag(6)-X%*%solve(t(X)%*%X)%*%t(X))%*%y
sig.hat

## i
solve(t(X)%*%X)[1,1] * 2.5^2
-qnorm(0.025)*sqrt(solve(t(X)%*%X)[1,1] * 2.5^2)

## j
-qt(0.025, 3)*sqrt(solve(t(X)%*%X)[1,1]) * sqrt(sig.hat/3)

## k
x0 <- rep(-1, 3)
y0 <- x0 %*% beta
y0