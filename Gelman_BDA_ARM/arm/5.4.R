library ("foreign")
wells <- read.table ("../doc/gelman/ARM_Data/arsenic/wells.dat", header=TRUE)
attach(wells)

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
print (summary(fit.1))

dist100 <- dist/100

fit.3 <- glm (switch ~ dist100 + arsenic, family=binomial(link="logit"))
print (summary(fit.3))
