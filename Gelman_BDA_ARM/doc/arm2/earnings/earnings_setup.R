library ("foreign")
heights <- read.dta ("heights.dta")
attach.all (heights)

# create variables for age and ethnicity categories

age <- 90 - yearbn                     # survey was conducted in 1990
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

# (for simplicity) remove cases with missing data
# and restrict to people with positive earnings born after 1925

ok <- !is.na (earn+height+sex+age) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age, age.category, eth, male)[ok,])
n <- nrow (heights.clean)

