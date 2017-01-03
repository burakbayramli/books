lda.fun <- function(y,X,x){
     n <- length(y)
     n0 <- sum(y==0)
     n1 <- sum(y==1)
     I1 <- (1:n)[y==1]
     I0 <- (1:n)[y==0]
     X1 <- X[I1,]
     X0 <- X[I0,]
     pi.hat <- n1/n
     mu1 <- apply(X1,2,mean); mu0 <- apply(X0,2,mean)
     S0 <- var(X0); S1 <- var(X1);
     S <- ((n0-1)*S0 + (n1-1)*S1)/n
     H <- solve(S)
     mu0 <- matrix(mu0,ncol=1);mu1 <- matrix(mu1,ncol=1)
     l0 <- (x %*% H %*% mu0) - c(.5*(t(mu0) %*% H %*% mu0)) + log(1-pi.hat)
     l1 <- (x %*% H %*% mu1) - c(.5*(t(mu1) %*% H %*% mu1)) + log(pi.hat)
     a <- .5*(t(mu1) %*% H %*% mu1) - .5*(t(mu0) %*% H %*% mu0) - 
           log(pi.hat/(1-pi.hat))
     b <- t(mu0 - mu1) %*% H 
     pred <- rep(0,nrow(x))
     pred[l1 > l0] <- 1
     list(pred=pred,pi.hat=pi.hat,mu0=mu0,mu1=mu1,S=S,a=a,b=b)
     }


qda.fun <- function(y,X,x){
     n <- length(y)
     n0 <- sum(y==0)
     n1 <- sum(y==1)
     I1 <- (1:n)[y==1]
     I0 <- (1:n)[y==0]
     X1 <- X[I1,]
     X0 <- X[I0,]
     pi0 <- n0/n
     pi1 <- n1/n
     mu1 <- apply(X1,2,mean); mu0 <- apply(X0,2,mean)
     S0 <- var(X0); S1 <- var(X1);
     H0 <- solve(S0);H1 <- solve(S1);
     mu0 <- matrix(mu0,ncol=1);mu1 <- matrix(mu1,ncol=1)
     d0 <- -.5*log(det(S0))-.5*mahalanobis(x,mu0,S0) + log(pi0)
     d1 <- -.5*log(det(S1))-.5*mahalanobis(x,mu1,S1) + log(pi1)

     pred <- rep(0,nrow(x))
     pred[d1 > d0] <- 1
     list(pred=pred,pi0=pi0,pi1=pi1,mu0=mu0,mu1=mu1,S0=S0,S1=S1)
     }



#postscript("fake.lda.ps")
#x1 <- c(runif(50,0,.3),runif(50,.7,1))
#x2 <- c(runif(50,0,.3),runif(50,.7,1))
#y <- rep(0,100)
#y[x2 < 1-x1] <- 1
#X <- cbind(x1,x2)
#out <- lda.fun(y,X,X)
#print(table(y,out$pred))
#plot(x1[y==1],x2[y==1],pch=2,lwd=3,col=2,xlab="",ylab="",
#     xlim=c(0,1),ylim=c(0,1))
#points(x1[y==0],x2[y==0],pch=15,lwd=3,col=3)
#a <- out$a; b <- out$b
#A <- -a/b[2]; B <- -b[1]/b[2]
#abline(A,B,col=1,lwd=3)
#dev.off()
#
#

postscript("south.africa.eps",horizontal=F,onefile=F,print.it=F)
par(pty="s")
x <- scan("sa.data",skip=1,sep=",")
x <- matrix(x,ncol=11,byrow=T)
chd <- x[,11]
y <- chd
J <- c(2,3)
X <- x[,J]
out <- lda.fun(chd,X,X)
print(table(chd,out$pred))
plot(x[,2][y==1],x[,3][y==1],pch=2,lwd=3,
     xlab="",ylab="",
     xlim=c(min(x[,2]),max(x[,2])),ylim=c(min(x[,3]),max(x[,3])))
points(x[,2][y==0],x[,3][y==0],pch=15,lwd=3)
a <- out$a; b <- out$b
A <- -a/b[2]; B <- -b[1]/b[2]
abline(A,B,col=1,lwd=3)
dev.off()

###QDA
#out <- qda.fun(chd,X,X)
#print(table(chd,out$pred))
##plot(x[,2][y==1],x[,3][y==1],pch=2,lwd=3,col=2,
##     xlab="Systolic Blood Pressure",ylab="Tobacco",
##     xlim=c(min(x[,2]),max(x[,2])),ylim=c(min(x[,3]),max(x[,3])))
##points(x[,2][y==0],x[,3][y==0],pch=15,lwd=3,col=3)
##a <- out$a; b <- out$b
##A <- -a/b[2]; B <- -b[1]/b[2]
##abline(A,B,col=1,lwd=3)
##
#
J <- c(2,3,4,6,8,9,10)
X <- x[,J]
out <- lda.fun(chd,X,X)
print(table(chd,out$pred))

J <- c(2,3,4,6,8,9,10)
X <- x[,J]
out <- qda.fun(chd,X,X)
print(table(chd,out$pred))

