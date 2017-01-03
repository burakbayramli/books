#2 (a)
z <- rnorm(10)
z

#(b)
epsilon <- 0.5*z

#(c)
x <- c(1,3,4,7,10,12,15,18,20,22)

#(d)
y <- 73-2*x+epsilon

#(e)
Ey <- 73- 2*x[1:4]

#(f)
ybar <- mean(y)
xbar <- mean(x)
n <- length(x)
Sxx <-sum(x^2) - (sum(x))^2/n
Sxy <- sum(x*y)- (sum(x)*sum(y))/n
beta1hat <- Sxy/Sxx
beta0hat <- ybar - beta1hat*xbar

#(g)
beta1hat
beta0hat

#(h)
yhat4 <- beta0hat + beta1hat *x[1:4]

#(i)
y[1:4]-yhat4

#(j)
Syy <-sum(y^2) - (sum(y))^2/n
sigmahat <- (Syy - beta1hat*Sxy)/(n-2)

#(k)
sdb1hat <- sqrt(sigmahat/Sxx)

#(l)
CIb1 <-beta1hat + qt(c(.025,.975), n-2)*sdb1hat

#(m)
yx4hat <- beta0hat + beta1hat*4
seyx4 <- sqrt((1/n + (4-xbar)^2/Sxx)*sigmahat)
CIyx4 <- yx4hat + qt(c(.025,.975),n-2)*seyx4

#(n)
seyx4pred <- sqrt((1+ 1/n + (4-xbar)^2/Sxx)*sigmahat)
CIyx4pred <- yx4hat + qt(c(.025,.975),n-2)*seyx4pred

#(o)
y.lm <- lm(y ~ x)

#(p)
summary(y.lm)

#(q)
fitted(y.lm)

#(r)
resid(y.lm)

#(s)
par(mfrow=c(2,1))
plot(x, epsilon)
plot(x, resid(y.lm))

#(t)
summary(y.lm)

#(v)
predict(y.lm, newdata=data.frame(x=4), interval="confidence")

#(w)
predict(y.lm, newdata=data.frame(x=4), interval="prediction")
