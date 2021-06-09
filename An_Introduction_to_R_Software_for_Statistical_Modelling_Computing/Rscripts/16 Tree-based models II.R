######################## © CSIRO Australia 2005 ###############################
# Session 16:  Tree Based Models:  Regression Trees & Advanced Topics         #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

####################
# Regression Trees
require(MASS)
require(rpart)
?Boston
boston.rp <- rpart(medv~.,method="anova",data=Boston,
    control=rpart.control(cp=0.0001))
summary(boston.rp)

# Complexity Table
printcp(boston.rp,digits=4)
plotcp(boston.rp)

# Pruning the Tree
boston.prune <- 	prune(boston.rp,cp=0.005)
plot(boston.prune)
text(boston.prune)

# Interactive Pruning
plot(boston.prune,uniform=T,branch=0.1)
text(boston.prune,pretty=1,use.n=T)
boston.prune.int <- snip.rpart(boston.prune)
plot(boston.prune.int,uniform=T,branch=0.1)
text(boston.prune.int,pretty=1,use.n=T)

# Forming Predictions
# Model 1
boston.pred1 <- predict(boston.prune)
# Model 2
boston.pred2 <- predict(boston.prune.int)
# Forming Correlation matrix to assess performance
boston.mat.pred <- cbind(Boston$medv,boston.pred1,boston.pred2)
boston.mat.pred <- data.frame(boston.mat.pred)
names(boston.mat.pred) <- c("medv","pred.m1","pred.m2")
cor(boston.mat.pred)
# Plotting
par(mfrow=c(1,2),pty="s")
with(boston.mat.pred,{
  eqscplot(pred.m1,medv,
    xlim=range(pred.m1,pred.m2),ylab="Actual",
    xlab="Predicted",main="Model 1")
  abline(0,1,col="blue",lty=5)
  eqscplot(pred.m2,medv,
    xlim=range(pred.m1,pred.m2),ylab="Actual",
    xlab="Predicted",main="Model 2")
  abline(0,1,col="blue",lty=5)
  par(mfrow=c(1,1))
 })

# Omitting rm (average number of rooms per dwelling)
boston.rp.omitRM <- update(boston.rp,~.-rm)
summary(boston.rp.omitRM)


# Examining Performance through a Test/Training Set
set.seed(1234)
n <- nrow(Boston)
# Sample 80% of the data
boston.samp <- sample(n, round(n*0.8))

bostonTrain <- Boston[boston.samp,]
bostonTest <- Boston[-boston.samp,]

testPred <- function(fit, data = bostonTest) {
#
# mean squared error for the performance of a
# predictor on the test data.
#
	testVals <- data[,"medv"]
	predVals <- predict(fit, data[, ])
	sqrt(sum((testVals - predVals)^2)/nrow(data))
}
# MSE on previous model
testPred(boston.prune,Boston)

# Fitting model to training set
bostonTrain.rp <- rpart(medv~.,data=bostonTrain,method="anova",cp=0.0001)
plotcp(bostonTrain.rp)
abline(v=7,lty=2,col="red")
bostonTrain.prune <- prune(bostonTrain.rp,cp=0.01)
plot(bostonTrain.prune,main="Training Dataset")
text(bostonTrain.prune)
# Computing the MSE
# Training dataset
testPred(bostonTrain.prune,bostonTrain)
# Test dataset
testPred(bostonTrain.prune,bostonTest)
# Forming Predictions
bostonTest.pred <- predict(bostonTrain.prune,bostonTest)
with(bostonTest,{
  cr <- range(bostonTest.pred,medv)
  eqscplot(bostonTest.pred,medv,
       xlim=cr,ylim=cr,
       ylab="Actual",xlab="Predicted",main="Test Dataset")
  abline(0,1,col="blue",lty=5)
})


########################
# Bagging

bsample <- function(dataFrame) # bootstrap sampling
dataFrame[sample(nrow(dataFrame), rep = T),  ]

simpleBagging <- function(object,
	data = eval(object$call$data), nBags = 200, ...) {
	bagsFull <- list()
	for(j in 1:nBags)
		bagsFull[[j]] <- update(object, data = bsample(data))
	oldClass(bagsFull) <- "bagRpart"
	bagsFull
}

predict.bagRpart <- function(object, newdata, ...)
	apply(sapply(object, predict, newdata = newdata), 1, mean)
	
# Execute bagging code on the Boston Housing Data
boston.bag <- simpleBagging(bostonTrain.rp)
testPred(boston.bag)
# Forming Predictions
boston.bag.pred <- predict(boston.bag, bostonTest)
# Forming Correlation matrix to assess performance
boston.mat.pred <- cbind(bostonTest$medv,bostonTest.pred,boston.bag.pred)
boston.mat.pred <- data.frame(boston.mat.pred)
names(boston.mat.pred) <- c("medv","pred.m1","pred.bag")
cor(boston.mat.pred)

# Plotting
with(bostonTest,{
  par(mfrow = c(2,2), pty = "s")
  frame()
  eqscplot(boston.bag.pred,medv,ylab="Actual",xlab="Predicted",
   main="Bagging")
  abline(0, 1, lty = 4, col = "blue")
  eqscplot(bostonTest.pred, medv, ylab="Actual",xlab="Predicted",
   main="Train/Test")
  abline(0, 1, lty = 4, col = "blue")
  with(Boston,
    eqscplot(boston.pred1,medv,ylab="Actual",xlab="Predicted",
       main="Resubstitution"))
  abline(0, 1, lty = 4, col = "blue")
  par(mfrow=c(1,1))
})



