library ("foreign")
wells <- read.table ("../doc/gelman/ARM_Data/arsenic/wells.dat", header=TRUE)
attach(wells)

dist100 <- dist/100
fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic,
  family=binomial(link="logit"))
print (fit.4)
