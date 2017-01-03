# Basic R Commands for Regression 

# lm(): linear model function, used for almost all regression fitting
#    that we will do
# y ~ x: This is a model formula relating y to x: y = beta0 + beta1*x
# data frames: rows of observations on one or more variables; usually
# one is the response variable. 

# Canadian Economic Data Example (available from data link on webpage)

# The following commands can be run from within R:

source("CanEconData.R")  # this brings the data frame into your session
# (You may also just cut and paste the dataset directly in.)

# Produce a scatter plot of inflation against rise in median salary:
plot(inflationrate ~ salaryrise, data=CanEconData)

# The following fits the regression model:
inflation.lm <- lm(inflationrate ~ salaryrise, data=CanEconData)

summary(inflation.lm) # this gives the output; pay most attention
# to the coefficients part of the output

abline(inflation.lm) # this places the fitted line on the scatter plot
