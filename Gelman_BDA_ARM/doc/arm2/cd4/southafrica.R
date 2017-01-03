# read in the data from an excel-format ".csv" file

hiv.data <- read.table ("allvar.csv", sep=",", header=T)
attach.all (hiv.data)

# use r to convert dates to "julian dates" (number of days since 1 jan 1960)

vdate <- as.character (strptime (as.character (VDATE), "%m/%d/%Y"))
v.year <- as.numeric (substr (vdate, 1, 4))
v.month <- as.numeric (substr (vdate, 6, 7))
v.day <- as.numeric (substr (vdate, 9, 10))
library (survival)
v.date <- as.numeric (mdy.date (v.month, v.day, v.year))
n.persons <- max(newpid)
initial.date <- rep (NA, n.persons)
for (j in 1:n.persons){
  initial.date[j] <- min (v.date[newpid==j], na.rm=T)
}
relative.date <- v.date - initial.date[newpid]

# just consider the "control" patients (treatmnt==1) and with initial age between 1 and 5 years

ok <- treatmnt==1 & !is.na(CD4PCT) & (baseage>1 & baseage<5)
ok[is.na(ok)] <- FALSE
y <- CD4PCT[ok]
time <- relative.date[ok]/365.24

pid.subset <- newpid[ok]
unique.pid <- unique (pid.subset)
n <- length (y)
J <- length (unique.pid)
person <- rep (NA, n)
for (j in 1:J){
  person[pid.subset==unique.pid[j]] <- j
}

# analyze cd4 on the square root scale!  (we'll discuss this)

y <- sqrt(y)

# ok, now the dataset is complete!  fit the bugs model

data <- list ("y", "person", "time", "n", "J")
inits <- function (){
  list (a=rnorm(J), b=runif(J), mu.a=rnorm(1), mu.b=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1))
}
params <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b")
sa.1 <- bugs (data, inits, params, "southafrica.1.txt", n.chains=3, n.iter=1000)

# check to see if the slopes and intercepts are strongly correlated (if so, we maybe should center the x-variable before including it in the regression)

# we'll plot 16 simulations of a,b

attach.bugs (sa.1)
par(mfrow=c(4,4))
for (i in 1:16)plot(a[i,],b[i,])
corrs <- rep (NA, n.sims)
for (i in 1:n.sims){
  corrs[i] <- cor (a[i,],b[i,])
}

# create a fake dataset from 400 new kids

n.reps <- 10
diff.summary <- array (NA, c(n.reps, 9))

for (rep in 1:n.reps){
  J.new <- 400
  T.new <- 7
  n.new <- J.new*T.new
  treatment.new <- rep (c(1,2), rep (J.new/2, 2))
  person.new <- rep (1:J.new, rep (T.new, J.new))
  time.new <- rep (seq(0,1,length=T.new), J.new)

  a.new <- rnorm (J.new, sa.1$mean$mu.a, sa.1$mean$sigma.a)
  b.new <- c (rnorm (J.new/2, sa.1$mean$mu.b, sa.1$mean$sigma.b),
              rnorm (J.new/2, sa.1$mean$mu.b/2, sa.1$mean$sigma.b/2))
  y.new <- rnorm (J.new*T.new, a.new[person.new] + b.new[person.new]*time.new,
                  sa.1$mean$sigma.y)

  # fit the new bugs model

  data <- list (y=y.new, person=person.new, time=time.new, n=n.new, J=J.new,
                treatment=treatment.new)
  inits <- function (){
    list (a=rnorm(J.new), b=runif(J.new), mu.a=rnorm(1), mu.b=rnorm(2),
          sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(2),
          xi.a=rnorm(1), xi.b=rnorm(2))
  }
  params <- c ("a.adj", "b.adj", "mu.a.adj", "mu.b.adj", "sigma.y", "sigma.a.adj", "sigma.b.adj", "diff")
  sa.new <- bugs (data, inits, params, "southafrica.new.1.bug", n.chains=3, n.iter=2000)

  diff.summary[rep,] <- summary["diff",]
}

# check corr between y_1 and regression

y.0.new <- y.new[time.new==0]
slope.new <- rep (NA, J.new)
for (j in 1:J.new){
  slope.new[j] <- lm (y.new ~ time.new, subset=person.new==j)$coef[2]
}

cor (y.0.new, slope.new)

