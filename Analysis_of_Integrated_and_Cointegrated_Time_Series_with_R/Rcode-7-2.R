beta <- H1@V
beta[,2] <- beta[,2]/beta[4,2]
beta[,3] <- beta[,3]/beta[4,3]
alpha <- H1@PI%*%solve(t(beta))
beta1 <- cbind(beta[,1:2], H1@V[,3:5]) 
ci.1 <- ts((H1@x%*%beta1)[-c(1,2),], start=c(1972, 3), end=c(1987, 2), frequency=4)
ci.2 <- ts(H1@RK%*%beta1, start=c(1972, 3), end=c(1987, 2), frequency=4)
