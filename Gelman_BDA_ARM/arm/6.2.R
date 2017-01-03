frisk <- read.table ("../doc/gelman/arm2/police/frisk_with_noise.dat", skip=6, header=TRUE)
attach(frisk)

for (i in 1:length(past.arrests)) {
  if (past.arrests[i] < 1) {
     past.arrests[i] = 0.00001
  }
}

log.arrests = log(past.arrests)

############# non-factor() usage for comparison to the Python version #########

res = glm(formula = stops ~ eth + log(past.arrests), family=poisson)
print (summary(res))

#(Intercept)        3.791960   0.017401   217.9   <2e-16 ***
#eth               -0.420116   0.004037  -104.1   <2e-16 ***
#log(past.arrests)  0.374052   0.002463   151.9   <2e-16 ***

############# factor usage, same code as in the book, ARM section 6.2 #########
############# but results could be different from the book due to     #########
############# manual changes in the data                              #########

glm.1 = glm(stops~1, family=poisson, offset=log(past.arrests))
print (summary(glm.1))

glm.2 = glm(stops ~ log.arrests, family=poisson)
print (summary(glm.2))

glm.3 = glm(formula = stops ~ factor(eth), family=poisson, offset=log(past.arrests))
print (summary(glm.3))

glm.4 = glm(formula = stops ~ factor(eth) + factor(precinct), family=poisson, offset=log(past.arrests))
print (summary(glm.4))

n = length(eth) # any param will do
print (n)
k = 3
yhat <- predict (glm.4, type="response")
z <- (stops-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")

glm.5 <- glm (stops ~ factor(eth) + factor(precinct) , family=quasipoisson, offset=log(past.arrests))
print (glm.5)

