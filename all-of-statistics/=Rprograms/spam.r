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
     a <- .5*(t(mu1) %*% H %*% mu1) -
           .5*(t(mu0) %*% H %*% mu0) - log(pi.hat/(1-pi.hat))
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





data <- scan("spam.data")
data <- matrix(data,ncol=58,byrow=T)
y <- data[,58]
x <- data[,1:57]
d <- data.frame(x,y)
out <- glm(y ~ .,family=binomial,data=d)
p <- out$fitted.values
pred <- rep(0,4601)
pred[p > .5] <- 1
e <- table(y,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)

out <- lda.fun(y,x,x)
e <- table(y,out$pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)

out <- qda.fun(y,x,x)
e <- table(y,out$pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)

library(tree)
postscript("spam.ps")
y <- as.factor(y)
d <- data.frame(x,y)
out <- tree(y ~ .,data=d)
print(summary(out))
plot(out,type="u",lwd=3)
text(out)
pred <- predict(out,d,type="class")
e <- table(y,pred)
print(e)
e <- (e[1,2]+e[2,1])/sum(e)
print(e)



cv.fun <- function(x,y,K=5){
     n <- nrow(x)
     b <- floor(n/K)
     I <- (1:n)
     I <- sample(I)
     I <- I[1:(K*b)]
     I <- matrix(I,b,K)
     Elda <- rep(0,K)
     Eqda <- rep(0,K)
     Elogistic <- rep(0,K)
     for(i in 1:K){
          exclude <- I[,i]
          include <- c(I[,-i])
          pred <- lda.fun(y[include],x[include,],x[exclude,])$pred
          e <- table(y[exclude],pred)
          e <- (e[1,2]+e[2,1])/sum(e)
          Elda[i] <- e

#          pred <- qda.fun(y[include],x[include,],x[exclude,])$pred
#          e <- table(y[exclude],pred)
#          e <- (e[1,2]+e[2,1])/sum(e)
#          Eqda[i] <- e
#
          xx <- x[include,];yy <- y[include]
          dd <- data.frame(xx,yy)
          out <- glm(yy ~ .,family=binomial,data=dd)
          new <- data.frame(x[exclude,],y[exclude])
          p <- predict.glm(out,newdata=new)
          pred <- rep(0,b)
          pred[p > .5] <- 1
          e <- table(y[exclude],pred)
          e <- (e[1,2]+e[2,1])/sum(e)
          Elogistic[i] <- e
          }
     Elda <- mean(Elda)
     Eqda <- mean(Eqda)
     Elogistic <- mean(Elogistic)
     list(Elda=Elda,Eqda=Eqda,Elogistic=Elogistic)
     }

out <- cv.fun(x,y,5)
print(out)







#cv <- cv.tree(out,FUN=prune.tree,K=10,method="misclass")
#size  <- cv$size
#score <- cv$dev
#dev.off()
#postscript("south.africa.tree.plot2.ps")
#plot(size,score,type="l",lwd=2,col=2)
#k     <- size[score==min(score)]
#k     <- k[1]
#new   <- prune.tree(out,best=k,method="misclass")
#print(summary(new))
#print(new)
#dev.off()
#postscript("south.africa.tree.plot3.ps")
#plot(new,lwd=3)
#text(new)


dev.off()


