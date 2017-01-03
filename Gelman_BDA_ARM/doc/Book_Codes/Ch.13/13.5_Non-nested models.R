## Read the pilots data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots
library("arm")
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

## Model fit
M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
display (M1)

## Plot figure 13.8

image (y.mat.new, col=gray((1:11)/12), xaxt="n", yaxt="n")
axis (2, seq(0,1,length=n.group), group.names[sort.group], tck=0, cex.axis=1.2)
axis (3, seq(0,1,length=n.scenario), scenario.abbr[sort.scenario], tck=0, cex.axis=1.2)
for (x in seq(.5,n.group-.5,1)/(n.group-1)) lines (c(-10,10),rep(x,2),col="white", lwd=.5)
for (x in seq(.5,n.scenario-.5,1)/(n.scenario-1)) lines (rep(x,2),c(-10,10),col="white", lwd=.5)

########################################################################################################
## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work

source("13.4_Understanding correlations between intercepts & slopes.R") # where data was cleaned

## Regression centering the predictors
x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)

M1 <- lmer (y ~ x.centered + (1 + x.centered | eth) + (1 + x.centered | age) +  
   (1 + x.centered | eth:age))
display (M1)

 # plot figure 13.10 ???????????

par (mfrow=c(3,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:3){
  for (k in 1:4){
     plot (x.jitter[age==j&eth==k], y[age==j&eth==k], xlab="height (inches from mean)", 
       ylab="log earnings", cex.lab=1.2, cex.axis=1.2, pch=20, cex=.6, cex.main=1.5,
       xlim=range(x), ylim=range(y), mgp=c(2,.7,0), yaxt="n", main=paste(eth.label[k], ", ",
     age.label[j], sep=""))
     axis (2, seq(6,12,2))
     a.00 <- height.2$median$b[1,j,k]
     b.00 <- height.2$median$b[2,j,k]
     for (i in 21:40)
       curve (b[i,1,j,k] + b[i,2,j,k]*x, lwd=.5, col="gray", add=T)
     curve (a.00 + b.00*x, lwd=2, add=T)
  }
}



