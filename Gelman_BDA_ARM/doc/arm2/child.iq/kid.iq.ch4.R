## code for nlsy kid iq data for Chapter 4

kidiq=read.dta("kidiq.dta")
attach(kidiq)

### Standardization using reasonable scales

display(lm(formula = kid.score ~ mom.hs + mom.iq + mom.hs:mom.iq))

c.mom.hs <- mom.hs - mean(mom.hs)
c.mom.iq <- mom.iq - mean(mom.iq)

display(lm(formula = kid.score ~ c.mom.hs + c.mom.iq + c.mom.hs:c.mom.iq))

c2.mom.hs <- mom.hs - 0.5
c2.mom.iq <- mom.iq - 100

display(lm(formula = kid.score ~ c2.mom.hs + c2.mom.iq + c2.mom.hs:c2.mom.iq))

z.mom.hs <- (mom.hs - mean(mom.hs))/(2*sd(mom.hs))
z.mom.iq <- (mom.iq - mean(mom.iq))/(2*sd(mom.iq))

display(lm(formula = kid.score ~ z.mom.hs + z.mom.iq + z.mom.hs:z.mom.iq))


### using discrete rather than continuous predictors

lm(formula = kid.score ~ as.factor(mom.work))


