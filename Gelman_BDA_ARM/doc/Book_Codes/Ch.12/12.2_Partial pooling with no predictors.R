## Read & clean the data
# get radon data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon
library ("arm")

srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"
radon <- srrs2$activity[mn]
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

 # no predictors
ybarbar = mean(y)

sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))
cty.mns = tapply(y,county,mean)
cty.vars = tapply(y,county,var)
cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size)
cty.sds.sep = sqrt(tapply(y,county,var)/sample.size)

 # varying-intercept model, no predictors

radon.data <- list ("n", "J", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a")

mlm.radon.nopred <- bugs (radon.data, radon.inits, radon.parameters,  n.chains=3, 
 "radon.multilevel.nopred.bug", bugs.directory="c:/.../",
    working.directory=NULL, clearWD=TRUE, n.iter=10, debug=TRUE)

## Figure 12.1 (a)

par(mfrow=c(1,2))
plot (sample.size.jittered, cty.mns, cex.lab=.9, cex.axis=1,
      xlab="sample size in county j",
      ylab="avg. log radon in county j",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,3.2), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         cty.mns[j] + c(-1,1)*cty.sds[j], lwd=.5)
#         cty.mns[j] + c(-1,1)*mean(cty.sds[!is.na(cty.sds)]), lwd=.5)
}
abline(h=mlm.radon.nopred$median$mu.a)
title("No pooling",cex.main=.9, line=1)
#abline(h=ybarbar)
points(sample.size.jittered[36],cty.mns[36],cex=4)

## Figure 12.1 (b)

plot (sample.size.jittered, mlm.radon.nopred$median$a, cex.lab=.9, cex.axis=1,
      xlab="sample size in county j",
      ylab="avg. log radon in county j",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,3.2), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         mlm.radon.nopred$median$a[j] + c(-1,1)*mlm.radon.nopred$sd$a[j],
      lwd=.5)
}
abline(h=mlm.radon.nopred$median$mu.a)
points(sample.size.jittered[36],mlm.radon.nopred$median$a[36],cex=4)#,col="red")
title("Multilevel model",cex.main=.9, line=1)








