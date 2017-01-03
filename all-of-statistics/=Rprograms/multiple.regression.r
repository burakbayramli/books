### multiple linear regression crime data
x <- scan("crime.dat",skip=1)
x <- matrix(x,ncol=14,byrow=T)
names <- c("Crime","Age","Southern","Education",
           "Expenditure","Ex1","Labor","Males",
           "pop","NW","U1","U2","Wealth","X")
### re-scale the data
for(i in 2:14){
     x[,i] <- x[,i]/sqrt(var(x[,i]))
     }
crime.dat <- as.data.frame(x)
names(crime.dat) <- names
out <- lm(Crime ~ Age + Southern + Education +
           Expenditure + Labor + Males +
           pop + U1 + U2 + Wealth, data=crime.dat)
print(summary(out))

print(step(out))

library(lasso2)

lambda <- seq(.1,.99,.05)
b  <- length(lambda)
cv <- rep(0,b)
beta <- matrix(0,10,b)
for (i in 1:b){
     temp <- l1ce(Crime ~ Age + Southern + Education +
           Expenditure + Labor + Males +
           pop + U1 + U2 + Wealth, data=crime.dat,bound=lambda[i])
    beta[,i] <- (temp$coeff)[2:11]
    score <- gcv.l1ce(temp,type="Tibshirani")
    cv[i] <- score[2]
    }

lambda.hat <- (lambda[cv==min(cv)])[1]
print(lambda.hat)

postscript("lasso.eps",
            horizontal = FALSE, onefile = FALSE, paper ="special",
           width=6,height=6)
par(mfrow=c(2,1))
plot(lambda,cv,xlab="",ylab="",type="l",lwd=3)
matplot(lambda,t(beta),type="b",lwd=3,
        xlab="",ylab="",lty=rep(1,10),pch=rep(19,10))
i <- ((1:b)[cv==min(cv)])[1]
print(i)
print(beta[,i])


dev.off()





