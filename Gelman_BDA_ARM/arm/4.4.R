library ("foreign")
heights <- read.dta ("../doc/gelman/ARM_Data/earnings/heights.dta")
attach (heights)

age <- 90 - yearbn
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

# (for simplicity) remove cases with missing data
# and restrict to people with positive earnings born after 1925

ok <- !is.na (earn+height+sex+age) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age, age.category, eth, male)[ok,])
n <- nrow (heights.clean)

attach (heights.clean)

log.earn <- log (earn)

earn.logmodel.1 <- lm (log.earn ~ height)

print (summary(earn.logmodel.1))

earn.logmodel.1 <- lm (log.earn ~ height + heights.clean$male)

print (summary(earn.logmodel.1))

earn.logmodel.3 <- lm (log.earn ~ heights.clean$height + heights.clean$male + heights.clean$height:heights.clean$male)

print (summary(earn.logmodel.3))
