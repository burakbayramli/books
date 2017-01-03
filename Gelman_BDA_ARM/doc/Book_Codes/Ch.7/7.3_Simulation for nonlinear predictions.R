## Read & clean the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/cong3

congress <- vector ("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- paste ("cong3/", year, ".asc", sep="")
  data.year <- matrix (scan (file), byrow=TRUE, ncol=5)
  data.year <- cbind (rep(year, nrow(data.year)), data.year)
  congress[[i]] <- data.year
}

# Note: download all ".asc" files into your R working directory in a file
# named cong3 for the above command to work

i86 <- (1986-1896)/2 + 1
cong86 <- congress[[i86]]
cong88 <- congress[[i86+1]]
cong90 <- congress[[i86+2]]

v86 <- cong86[,5]/(cong86[,5]+cong86[,6])
bad86 <- cong86[,5]==-9 | cong86[,6]==-9
v86[bad86] <- NA
contested86 <- v86>.1 & v86<.9
inc86 <- cong86[,4]

v88 <- cong88[,5]/(cong88[,5]+cong88[,6])
bad88 <- cong88[,5]==-9 | cong88[,6]==-9
v88[bad88] <- NA
contested88 <- v88>.1 & v88<.9
inc88 <- cong88[,4]

v90 <- cong90[,5]/(cong90[,5]+cong90[,6])
bad90 <- cong90[,5]==-9 | cong90[,6]==-9
v90[bad90] <- NA
contested90 <- v90>.1 & v90<.9
inc90 <- cong90[,4]

jitt <- function (x,delta) {x + runif(length(x), -delta, delta)}

## Plot Figure 7.3

v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist, breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=1.1, cex.lab=1.1, cex.main=1.2, 
  main="Congressional elections in 1988")

## Fitting the model

v86.adjusted <- ifelse (v86<.1, .25, ifelse (v86>.9, .75, v86))
vote.86 <- v86.adjusted[contested88]
incumbency.88 <- inc88[contested88]
vote.88 <- v88[contested88]

library ("arm")
fit.88 <- lm (vote.88 ~ vote.86 + incumbency.88)
display (fit.88)

## Figure 7.4

 # 7.4 (a)

par (mfrow=c(1,1))
par (pty="s", mar=c(5,5,4,1)+.1)
plot (0, 0, xlim=c(0,1), ylim=c(0,1), type="n",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
  cex.lab=1)
abline (0,1, lwd=.5)
j.v86 <- ifelse (contested86, v86, jitt (v86, .02))
j.v88 <- ifelse (contested88, v88, jitt (v88, .02))
points (j.v86[inc88==0], j.v88[inc88==0], pch=1)
points (j.v86[inc88==1], j.v88[inc88==1], pch=16)
points (j.v86[inc88==-1], j.v88[inc88==-1], pch=4)
mtext ("Raw data (jittered at 0 and 1)", line=1, cex=1.2)

 # 7.4 (b)

par (pty="s", mar=c(5,5,4,1)+.1)
plot (0, 0, xlim=c(0,1), ylim=c(0,1), type="n",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
  cex.lab=1)
abline (0,1, lwd=.5)
v86.adjusted <- ifelse (v86<.1, .25, ifelse (v86>.9, .75, v86))
vote.86 <- v86.adjusted[contested88]
vote.88 <- v88[contested88]
incumbency.88 <- inc88[contested88]
points (vote.86[incumbency.88==0], vote.88[incumbency.88==0], pch=1)
points (vote.86[incumbency.88==1], vote.88[incumbency.88==1], pch=16)
points (vote.86[incumbency.88==-1], vote.88[incumbency.88==-1], pch=4)
mtext ("Adjusted data (imputing 0's and 1's to .75)", line=1, cex=1.2)

## Simulation for inferences and predictions of new data points

incumbency.90 <- inc90
vote.88 <- v88
n.tilde <- length (vote.88)
X.tilde <- cbind (rep (1, n.tilde), vote.88, incumbency.90)

n.sims <- 1000
sim.88 <- sim (fit.88, n.sims)
y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  pred <- X.tilde %*% sim.88$coef[s,]
  ok <- !is.na(pred)
  y.tilde[s,ok] <- rnorm (sum(ok), pred[ok], sim.88$sigma[s])
}

## Predictive simulation for a nonlinear function of new data

y.tilde.new <- ifelse (y.tilde=="NaN", 0, y.tilde)

dems.tilde <- rowSums (y.tilde.new > .5)
 # or
dems.tilde <- rep (NA, n.sims)
for (s in 1:n.sims){
  dems.tilde[s] <- sum (y.tilde.new[s,] > .5)
}

## Implementation using functions

Pred.88 <- function (X.pred, lm.fit){
  sim.88 <- sim (lm.fit, 1)
  pred <- X.tilde %*% t(sim.88$coef)
  ok <- !is.na(pred)
  n.pred <- length (pred)
  y.pred <- rep (NA, n.pred)
  y.pred[ok] <- rnorm (sum(ok), pred[ok], sim.88$sigma)
  return (y.pred)
}

y.tilde <- replicate (1000, Pred.88 (X.tilde, fit.88))
dems.tilde <- replicate (1000, Pred.88 (X.tilde, fit.88) > .5)
