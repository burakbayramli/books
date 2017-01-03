X <- scan("sa.data",skip=1,sep=",")
X <- matrix(X,ncol=11,byrow=T)
chd <- X[,11]
n <- length(chd)
X <- X[,-c(1,11)]
names <- c("sbp","tobacco","ldl","adiposity","famhist","typea","obesity","alcohol","age")
for(i in 1:9){
     assign(names[i],X[,i])
     }

formula <- paste(names,sep="",collapse="+")
formula <- paste("chd ~ ",formula)
formula <- as.formula(formula)


out <- lm(formula)
print(summary(out))
p <- out$fitted.values
names(p) <- NULL
predict <- rep(0,n)
predict[p > .5] <- 1
print(table(chd,predict))


out <- glm(formula,family=binomial)
print(summary(out))
p <- out$fitted.values
names(p) <- NULL
predict <- rep(0,n)
predict[p > .5] <- 1
print(table(chd,predict))

error <- sum( ((chd==1)&(predict==0)) | ((chd==0)&(predict==1)) )/n
print(error)


######## Now do quadratic version
k <- ncol(X)
XX <- X
for(i in 1:k){
     for(j in i:k){
          temp <- X[,i]*X[,j]
          XX <- cbind(XX,temp)
          }
     }
kk <- ncol(XX)
colnames(XX) <- paste("x",1:kk)
out <- glm.fit(XX,chd,family=binomial())
p <- out$fitted.values
names(p) <- NULL
predict <- rep(0,n)
predict[p > .5] <- 1
print(table(chd,predict))

error <- sum( ((chd==1)&(predict==0)) | ((chd==0)&(predict==1)) )/n
print(error)
