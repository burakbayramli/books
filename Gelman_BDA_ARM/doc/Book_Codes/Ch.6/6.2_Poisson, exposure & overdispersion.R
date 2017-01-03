## Read in the data

missing data files?

## Model fitting

 # constant term

fit.1 <- glm (stops ~ 1, family=poisson, offset=log(arrests))
display(fit.1)

 # ethnicity indicator

fit.2 <- glm (stops ~ factor(eth), family=poisson, offset=log(arrests))
display(fit.2)

 # ethnicity & precints indicators

fit.3 <- glm (stops ~ factor(eth) + factor(precints) , family=poisson,
   offset=log(arrests))
display(fit.3)

 # overdispersion

fit.4 <- glm (stops ~ factor(eth) + factor(precints) , family=quasipoisson,
   offset=log(arrests))
display(fit.4)




