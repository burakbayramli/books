######################## © CSIRO Australia 2005 ###############################
# Session 14:  Neural Networks: An Introduction                               #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

#######################
# Regression using Neural Networks

# Cars93 Data
# Linear Regression:  nnet
library(nnet)
tst <- nnet(1000/MPG.city ~ Weight+Origin+Type, Cars93, linout = T,
   size = 0, decay = 0, rang = 0, skip = T, trace = T)
coef(tst)

# Linear Regression:  lm
tst1 <- lm(1000/MPG.city ~ Weight + Origin+Type, Cars93)
coef(tst1)
range(coef(tst) - coef(tst1))

# Birthweight Data
# Logistic Regression:  nnet
tst <- nnet(low ~ ptd + ftv/age, data = bwt,skip = T, size = 0, decay = 0,
  rang = 0, trace = T)

# Logistic Regression:  glm
tst1 <- glm(low ~ ptd + ftv/age, binomial, bwt)
range(coef(tst) - coef(tst1))

# Set up a training and test set
sb1 <- sample(1:nrow(bwt), 100)
bwt.train <- bwt[sb1,  ]
bwt.test <- bwt[ - sb1,  ]
bm.train <- update(tst1, data = bwt.train)
bm.tst <- predict(bm.train, bwt.test, type = "resp")
bm.class <- round(bm.tst)
table(bm.class, bwt.test$low)

# Classification Tree Model
library(tree)
tm.train <- tree(low ~ race + smoke + age + ptd + ht + ui + ftv, bwt.train)
plot(cv.tree(tm.train, FUN = prune.misclass))
tm.train <- prune.tree(tm.train, best = 6)
tm.class <- predict(tm.train, bwt.test, type = "class")
table(tm.class, bwt.test$low)

# Initial Explorations of a Neural Network
X0 <- model.matrix( ~ race + smoke + age + ptd + ht + ui + ftv, bwt.train)[, -1]
std <- function(x) (x - min(x))/(max(x) - min(x))
X <- apply(X0, 2, std)
nm.train <- nnet(low ~ X, data = bwt.train, size = 3,  skip = T,
         decay = 0.001, trace = T, maxit = 1000)
         
# Test Data
X <- model.matrix( ~ race + smoke + age + ptd + ht + ui + ftv, bwt.test)[, -1]
for(j in 1:ncol(X))
  X[, j] <- (X[, j] - min(X0[, j]))/(max(X0[, j]) - min(X0[, j]))
nm.tst <- predict(nm.train, newdata = bwt.test, type = "raw")
nm.class <- round(nm.tst)
table(nm.class, bwt.test$low)


