cv.fun <- function(x,y,K=10){
     x <- as.matrix(x)
     n <- nrow(x)
     k <- ncol(x)
     junk <- paste("x",1:k)
     colnames(x) <- junk
     m <- floor(n/K)
     I <- sample(1:n)
     error <- rep(0,K)
     for(i in 1:K){
          ind <- I[ (m*(i-1)+1):(m*i) ]
          if(i==K) ind <- I[ (m*(i-1)+1):n ]
          xx <- as.matrix(x[-ind,])
          yy <- y[-ind]
          colnames(xx) <- junk
          out <- glm.fit(xx,yy,family=binomial())
          beta <- out$coef
          beta <- matrix(beta,ncol=1)
          p <- x[ind,] %*% beta
          p <- exp(p)/(1+exp(p))
          predict <- rep(0,length(p))
          predict[p > .5] <- 1
          error[i] <- sum( ((y[ind]==1)&(predict==0)) | ((y[ind]==0)&(predict==1)) )
          error[i] <- error[i]/length(p)
          }
     error <- mean(error)
     error
     }





X <- scan("sa.data",skip=1,sep=",")
X <- matrix(X,ncol=11,byrow=T)
y <- X[,11]
n <- length(y)
X <- X[,-c(1,11)]


x <- X[,1]
x <- matrix(x,ncol=1)
colnames(x) <- "x1"
R <- 9
error <- rep(0,2*R)
cv <- error
out <- glm.fit(x,y,family=binomial())
p <- out$fitted.values
names(p) <- NULL
predict <- rep(0,n)
predict[p > .5] <- 1
error[1] <- sum( ((y==1)&(predict==0)) | ((y==0)&(predict==1)) )/n
cv[1] <- cv.fun(x,y)
for(r in 2:R){
     cat("r = ",r,"\n")
     x <- cbind(x,X[,r])
     colnames(x) <- paste("x",1:r)
     out <- glm.fit(x,y,family=binomial())
     p <- out$fitted.values
     names(p) <- NULL
     predict <- rep(0,n)
     predict[p > .5] <- 1
     error[r] <- sum( ((y==1)&(predict==0)) | ((y==0)&(predict==1)) )/n
     cv[r] <- cv.fun(x,y)
     }

for(r in 1:R){
     cat("r = ",r+9,"\n")
     x <- cbind(x,X[,r]^2)
     colnames(x) <- c(paste("x",1:R),paste("xx",1:r))
     out <- glm.fit(x,y,family=binomial())
     p <- out$fitted.values
     names(p) <- NULL
     predict <- rep(0,n)
     predict[p > .5] <- 1
     error[r+9] <- sum( ((y==1)&(predict==0)) | ((y==0)&(predict==1)) )/n
     cv[r+9] <- cv.fun(x,y)
     }


postscript("cv.ps")

a <- min(c(error,cv))
b <- max(c(error,cv))
plot(1:(2*R),error,xlab="Number of terms in the model",ylab="error rate",
     type="l",lwd=3,ylim=c(a,b))
lines(1:(2*R),cv,lty=2,col=2,lwd=3)
print(cbind(1:(2*R),error))
print(cbind(1:(2*R),cv))

dev.off()






