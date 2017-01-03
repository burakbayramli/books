library(MPV)  # this contains the cement data set
library(leaps)   # this contains the variable selection routines
data(cement)
x <- cement[,-1]  # this removes the y variable from
                   # the cement data set
y <- cement[,1]  # this is the y variable
cement.leaps <- leaps(x,y)
attach(cement.leaps)  # this allows us to access
                       # the variables in cement.leaps
                      # directly
                      # e.g., Mallows Cp is one of the
                      # variables calculated

plot(size, Cp)      # size = no. of parameters
abline(0,1)         # reference line to see where Cp = p
identify(size, Cp)  # which models are close to Cp = p?
                      # Click on the
                      # plotted points near the reference line.
which[6,]       # which variables are included in model 6?

# Variables 1 and 4 are in the model.

cement.6 <- lm(y ~ as.matrix(x[,which[6,]]))
summary(cement.6)

PRESS(cement.6)

# What about model 14?

cement.14 <- lm(y ~ as.matrix(x[,which[14,]]))
summary(cement.14)

PRESS(cement.14)


# What about Model 15?  (This is the full model.)

cement.15 <- lm(y ~ as.matrix(x[,which[15,]]))
summary(cement.15)

PRESS(cement.15)

plot(cement.15) 