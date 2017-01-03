## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/?
?data

library ("arm")

attach.all ()

## Computation of propensity score matches

 # starting point

ps.fit.1 <- glm (treat ~ as.factor(educ) + as.factor(ethnic) + b.marr +
   work.dur + prenatal + mom.age + sex + first + preterm + age + 
   dayskidh + bw + unemp.rt, data=cc2, family=binomial(link="logit"))

 # second model

ps.fit.2 <- glm (treat ~ bwg + as.factor(educ) + bwg:as.factor(educ) +
   as.factor(ethnic) + b.marr + as.factor(ethnic):b.marr + work.dur + 
   prenatal + preterm + age + mom.age + sex + first, data=cc2,
   family=binomial(link="logit"))

 # predicted values

pscores <- matching (z=cc2$treat, score=pscores)
matched <- cc2[matches$matched,]

 # regression on matched data

reg.ps <- lm (ppvtr.36 ~ treat + hispanic + black + b.marr + lths + hs +
   ltcoll + work.dur + prenatal + mom.age + sex + first + preterm + age +
   dayskidh + bw, data=matched)

## Propensity score as a one-number summary (Figure 10.7)







