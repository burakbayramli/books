a4.1 <-
function (id, answer)
{
set.seed(id)
N1 <- rpois(1, 5)+4
vo <- seq(4,72,4)
vo <- c(vo, vo[N1]+round(rnorm(1,mean=0,sd=2),1))
a <- -rnorm(1,mean=4,sd=.25) -.1*(max(vo-25,0)^.25)
To <- .4
Ts <- 1
vskid <- 70*5/18
sigma1 <- .25
sigma2 <- .05
sigma3 <- .35
sigma4 <- .05
vo <- 5*vo/18
n <- length(vo)
eps1 <- rnorm(n,sd=sigma1)
eps2 <- rnorm(n,sd=sigma2)
eps3 <- rnorm(n,sd=sigma3)
eps4 <- rnorm(n,sd=sigma4)
basic <- (vo+eps1)*(To+eps2)-(vo+eps1)^2/(2*(a+eps3))
d <-  basic + ((vo+eps1)>=vskid)*((vo+eps1)*(Ts+eps4))
d[N1] <- d[N1]*(1-2*rbinom(1,1,.3))
d[!is.na(d) & d<3] <- NA
if (runif(1) < .3) {d[N1-1]<-d[N1]<-NA}
else {if (runif(1) < .35) {d[N1] <- 0}
else {d[N1] <- d[N1]/2}}
if (!missing(answer)) {list(stopping.distance=d[n],
mark = 100-3*(d[n]-answer[1])^2)
}
else {
print(paste("Predict stopping distance at an initial velocity of",vo[n]*18/5, "km/h, using:"))
data.frame(initial.velocity=vo[-n]*18/5, stopping.distance=round(d[-n],1))
}
}

a4.2 <-
function (id, answer)
{
set.seed(id)
N1 <- rpois(1, 5)+4
vo <- seq(4,72,4)
vo <- c(vo, vo[N1]+round(rnorm(1,mean=0,sd=2),1))
n <- length(vo)
wet <- rbinom(n, 1, .5)
temperature <- round(rnorm(n, mean=18, sd=1.5),1)
a <- -rnorm(n,mean=4*(1-wet)+2*wet,sd=.25)
To <- .4
Ts <- 1
vskid <- 70*5/18
sigma1 <- .25
sigma2 <- .05
sigma3 <- .35
sigma4 <- .05
vo <- 5*vo/18
eps1 <- rnorm(n,sd=sigma1)
eps2 <- rnorm(n,sd=sigma2)
eps3 <- rnorm(n,sd=sigma3)
eps4 <- rnorm(n,sd=sigma4)
basic <- (vo+eps1)*(To+eps2)-(vo+eps1)^2/(2*(a+eps3))
d <-  basic + ((vo+eps1)>=vskid)*((vo+eps1)*(Ts+eps4))
d[N1] <- d[N1]*(1-2*rbinom(1,1,.3))
if (runif(1) < .3) {d[N1]<--2*d[N1]}
else {if (runif(1) < .35) {d[N1] <- 0}
else {d[N1] <- d[N1]/2}}
if (!missing(answer)) {list(stopping.distance=d[n],
mark = round((100-1.5*(d[n]-answer[1])^2)/2+(100*(answer[3]>d[n])*(answer[2]<d[n]))/2,0))
}
else {
pavement <- factor(wet)
levels(pavement)<- c("dry", "wet")
print(paste("Predict stopping distance at initial.velocity=",vo[n]*18/5,
", pavement = ",pavement[n],
", temperature = ", temperature[n], sep=""))
data.frame(initial.velocity=vo[-n]*18/5, stopping.distance=round(d[-n],1),
pavement=pavement[-n], temperature=temperature[-n])
}
}

