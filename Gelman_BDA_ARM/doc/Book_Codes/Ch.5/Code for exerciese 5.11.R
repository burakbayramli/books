## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

# The R codes & data files should be saved in the same directory for
# the source command to work

source("4.7_Fitting a series of regressions.R") # where data was cleaned

## Using "black" as a predictor for exercise 11

vote <- rvote

glm.1 <- glm (rvote ~ female + black + income, subset=(nes.year==1960), family=binomial(link="logit"))
display (glm.1)
glm.2 <- glm (rvote ~ female + black + income, subset=(nes.year==1964), family=binomial(link="logit"))
display (glm.2)
glm.3 <- glm (rvote ~ female + black + income, subset=(nes.year==1968), family=binomial(link="logit"))
display (glm.3)
glm.4 <- glm (rvote ~ female + black + income, subset=(nes.year==1972), family=binomial(link="logit"))
display (glm.4)








