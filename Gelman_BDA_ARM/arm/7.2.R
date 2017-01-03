library ("foreign")
library ("arm")

heights <- read.dta ("../doc/gelman/ARM_Data/earnings/heights.dta")
attach (heights)

age <- 90 - yearbn
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

ok <- !is.na (earn+height+sex+age) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age, age.category, eth, male)[ok,])
n <- nrow (heights.clean)

attach (heights.clean)

log.earn <- log (earn)

z.height <- (heights.clean$height - mean(heights.clean$height))/sd(heights.clean$height)

earn.logmodel.3 <- lm(formula = log.earn ~ z.height + heights.clean$male + z.height:heights.clean$male)

print (summary(earn.logmodel.3))

n.sims <- 1000
fit.1 <- lm (log.earn ~ height + heights.clean$male + heights.clean$height:heights.clean$male)
sim.1 <- sim (fit.1, n.sims)

# results should be same as 4.4
height.coef <- sim.1$coef[,2]
mean (height.coef)
sd (height.coef)
quantile (height.coef, c(.025,.975))

# simulate 
height.for.men.coef <- sim.1$coef[,2] + sim.1$coef[,4]
quantile (height.for.men.coef, c(.025,.975))
