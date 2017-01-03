# load in and plot the flight simulator data

pilots <- read.table ("pilots.dat", header=TRUE)
attach.all (pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- group==group.names[j] & scenario==scenario.names[k]    
    successes <- c (successes, sum(recovered[ok]==1,na.rm=T))
    failures <- c (failures, sum(recovered[ok]==0,na.rm=T))
    group.id <- c (group.id, j)
    scenario.id <- c (scenario.id, k)
  }
}
y <- successes/(successes+failures)
y.mat <- matrix (y, n.scenario, n.group)
sort.group <- order(apply(y.mat,2,mean))
sort.scenario <- order(apply(y.mat,1,mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario,sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

# plot the data
image (y.mat.new, col=gray((1:11)/12), xaxt="n", yaxt="n")
axis (2, seq(0,1,length=n.group), group.names[sort.group], tck=0, cex.axis=1.8)
axis (3, seq(0,1,length=n.scenario), scenario.abbr[sort.scenario], tck=0, cex.axis=1.7)
for (x in seq(.5,n.group-.5,1)/(n.group-1)) lines (c(-10,10),rep(x,2),col="white", lwd=.5)
for (x in seq(.5,n.scenario-.5,1)/(n.scenario-1)) lines (rep(x,2),c(-10,10),col="white", lwd=.5)

# fit the 2-way-model using lmer

treatment <- group.id
airport <- scenario.id

pilots1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
display (pilots1)

# Add a noise predictor
x <- rnorm(40)
pilots2 <- lmer (y ~ 1 + x + (1 | group.id) + (1 | scenario.id))

# super-simple lmer test

# set up the predictors

n.groups <- 10
n.reps <- 2
n <- n.groups*n.reps
group.id <- rep (1:n.groups, each=n.reps)

# simulate the varying parameters

a.group <- rnorm (n.groups, 1, 2)

# simulate the data and print to check that i did it right

y <- rnorm (n, a.group[group.id], 1)
print (cbind (y, group.id))

# fit and summarize the model

fit.1 <- lmer (y ~ 1 + (1 | group.id))
display (fit.1)

# add another predictor
x <- rnorm (n)
fit.2 <- lmer (y ~ 1 + x + (1 | group.id))
display (fit.2)

# simple lmer test

# set up the predictors

n.groups <- 5
n.scenarios <- 8
n.reps <- 2
n <- n.groups*n.scenarios*n.reps
group.id <- rep (1:n.groups, each=n.scenarios*n.reps)
scenario.id <- rep (1:n.scenarios, n.groups, each=n.reps)

# simulate the varying parameters

a.group <- rnorm (n.groups, 1, 2)
a.scenario <- rnorm (n.scenarios, 3, 4)
a.group.scenario <- array (rnorm (n.groups*n.scenarios, 0, 1), c(n.groups, n.scenarios))

# simulate the data and print to check that i did it right

y <- rnorm (n, a.group[group.id] + a.scenario[scenario.id] + a.group.scenario[group.id,scenario.id], 1)
print (cbind (y, group.id, scenario.id))

# fit and summarize the model

fit.1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
display (fit.1)

# add another predictor
x <- rnorm (n)
fit.2 <- lmer (y ~ 1 + x + (1 | group.id) + (1 | scenario.id))
display (fit.2)


# fit the 2-way-model using bugs

n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), mu.treatment=rnorm(1), mu.airport=rnorm(1), b.treatment=rnorm(n.treatment), b.airport=rnorm(n.airport), sigma.treatment=runif(1), sigma.airport=runif(1), sigma.y=runif(1))
}
parameters <- c("g.mu", "g.treatment", "g.airport", "sigma.treatment", "sigma.airport", "sigma.y", "s.treatment", "s.airport", "s.error", "mu", "gg.mu", "gg.treatment", "gg.airport")
pilots.1 <- bugs (data, inits, parameters, "pilots.1.bug", n.chains=3, n.iter=1000)

anova.plot (pilots.1$summary, c("treatment","airport","error"),
            c(n.treatment-1,n.airport-1,n-1-(n.treatment-1)-(n.airport-1)),
            file="c:/books/multilevel/pilotanova.ps")

# classical anova of pilots data

summary (aov (y ~ factor (treatment) + factor(airport)))

# compare inferences for finite-pop, superpop sd's

source("anova.R")

anova.plot0 (pilots.1$summary, c("treatment","airport","y"),
            c(n.treatment-1,n.airport-1,n-1-(n.treatment-1)-(n.airport-1)),
            file="c:/books/multilevel/pilotanova.0a.ps", x.max=.85, signame="sigma.", sourcenames=c(expression(sigma[gamma]), expression(sigma[delta]), expression(sigma[y])), plot.title="superpopulation s.d.'s")

anova.plot0 (pilots.1$summary, c("treatment","airport","error"),
            c(n.treatment-1,n.airport-1,n-1-(n.treatment-1)-(n.airport-1)),
            file="c:/books/multilevel/pilotanova.0b.ps", x.max=.85, signame="s.", sourcenames=c(expression(s[gamma]), expression(s[delta]), expression(s[y])), plot.title="finite-population s.d.'s")


# compare inferences for uncentered, centered parameter ests

postscript ("c:/books/multilevel/pilots.centering.a.ps", horizontal=T)
par (mar=c(7,8,4,2)+.1)
plot (0, 0, xlab="airport, k", ylab=expression(delta[k]), main=expression (paste ("uncentered parameters,  ", delta[k])), xlim=c(1,n.airport), ylim=c(-.65,.65),
      pch=20, cex.axis=3, cex.lab=3, cex=3, cex.main=3, type="n", mgp=c(5,2,0), yaxt="n")
axis (2, c(-.5,0,.5), cex.axis=3)
for (j in 1:n.airport){
  jj <- sort.scenario[j]
  points (j, median (gg.airport[,jj]), pch=20, cex=2)
  lines (rep(j,2), quantile(gg.airport[,jj],c(.16,.84)), lwd=.5)
}
dev.off()


postscript ("c:/books/multilevel/pilots.centering.b.ps", horizontal=T)
par (mar=c(7,8,4,2)+.1)
plot (0, 0, xlab="airport, k", ylab=expression(delta[k]^adj), main=expression (paste ("zero-centered parameters,  ", delta[k]^adj)), xlim=c(1,n.airport), ylim=c(-.65,.65),
      pch=20, cex.axis=3, cex.lab=3, cex=3, cex.main=3, type="n", mgp=c(5,2,0), yaxt="n")
axis (2, c(-.5,0,.5), cex.axis=3)
for (j in 1:n.airport){
  jj <- sort.scenario[j]
  points (j, median (g.airport[,jj]), pch=20, cex=2)
  lines (rep(j,2), quantile(g.airport[,jj],c(.16,.84)), lwd=.5)
}
dev.off()

jitter.centered <- function (a, delta=.2){
  j1 <- a + runif (length(a), -delta, delta)
  j1 + mean(a) - mean(j1)
}

postscript ("c:/books/multilevel/pilots.centering.2.ps", horizontal=T)
par (mar=c(7,8,4,2)+.1)
plot (0, 0, xlab="airport, k", ylab=expression(mu + bar(gamma) + delta[k]), main=expression (paste ("intercept-shifted parameters,  ", mu + bar(gamma) + delta[k])), xlim=c(1,n.airport), ylim=c(0,1.05),
      pch=20, cex.axis=3, cex.lab=3, cex=3, cex.main=3, type="n", mgp=c(5,2,0), yaxt="n")
axis (2, c(0,.5,1), cex.axis=3)
for (j in 1:n.airport){
  jj <- sort.scenario[j]
  points (j, median (g.mu + g.airport[,jj]), pch=20, cex=2)
  lines (rep(j,2), quantile(g.mu + g.airport[,jj],c(.16,.84)), lwd=.5)
  points (jitter.centered(rep(j,5)), y.mat[jj,], pch=20, cex=1)
}
dev.off()

