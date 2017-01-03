# Read in the data from an excel-format ".csv" file

hiv.data <- read.csv ("allvar.csv")
attach.all (hiv.data)

# just consider the "control" patients (treatmnt==1) and with initial age between 1 and 5 years

ok <- treatmnt==1 & !is.na(CD4PCT) & (baseage>1 & baseage<5)
attach.all (hiv.data[ok,])

y <- sqrt (CD4PCT)
age.baseline <- baseage        # kid's age (yrs) at the beginning of the study
age.measurement <- visage      # kids age (yrs) at the time of measurement
treatment <- treatmnt
time <- visage - baseage

# set up new patient id numbers from 1 to J

unique.pid <- unique (newpid)
n <- length (y)
J <- length (unique.pid)
person <- rep (NA, n)
for (j in 1:J){
  person[newpid==unique.pid[j]] <- j
}

# Person-level summaries

number.of.measurements <- as.vector (table (patient.id))
patient.age.baseline <- baseage [age.baseline==age.measurement]

# fit multilevel model using lmer

M1 <- lmer (y ~ time + (1 + time | person))
display (M1)

# fit multilevel model using bugs

data <- list ("y", "person", "time", "n", "J")
inits <- function (){
  list (a=rnorm(J), b=runif(J), mu.a=rnorm(1), mu.b=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1))
}
params <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b")
M1.bugs <- bugs (data, inits, params, "southafrica.1.bug", n.chains=3, n.iter=1000, debug=TRUE)
