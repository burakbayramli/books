library("arm")
hiv.data <- read.csv ("../doc/gelman/ARM_Data/cd4/allvar.csv")
attach.all (hiv.data)

## Eliminate missing data & just consider the "control" patients (treatmnt==1)
## and with initial age between 1 and 5 years
ok <- treatmnt==1 & !is.na(CD4PCT) & (baseage>1 & baseage<5)
attach.all (hiv.data[ok,])

## Redefining variables
y <- sqrt (CD4PCT)
age.baseline <- baseage        # kid's age (yrs) at the beginning of the study
age.measurement <- visage      # kids age (yrs) at the time of measurement
treatment <- treatmnt
time <- visage - baseage

## Set up new patient id numbers from 1 to J
unique.pid <- unique (newpid)
n <- length (y)
J <- length (unique.pid)
person <- rep (NA, n)
for (j in 1:J){
person[newpid==unique.pid[j]] <- j
}

## Fit the model
M1 <- lmer (y ~ time + (1 + time | person))
display (M1)

