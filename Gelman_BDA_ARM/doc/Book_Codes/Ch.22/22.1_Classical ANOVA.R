## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots

# The R codes & data files should be saved in the same directory for
# the source command to work

source("13.5_Non-nested models.R") # where data was cleaned

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Classical anova of pilots data
summary (aov (y ~ factor (treatment) + factor(airport)))



