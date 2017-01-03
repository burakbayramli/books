# For week 2 of statistical computing class

# Simulation example from section 7.2 of ARM

congress <- vector ("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- paste ("cong3/", year, ".asc", sep="")
  data.year <- matrix (scan (file), byrow=TRUE, ncol=5)
  data.year <- cbind (rep(year, nrow(data.year)), data.year)
  congress[[i]] <- data.year
}

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

#postscript ("hist88.ps", horizontal=T)    # un-comment this to save as a postscript file
v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist, breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Congressional elections in 1988")
#dev.off()       # un-comment this if you've put the graph in a postscript file

#postscript ("hist88a.ps", horizontal=T)
v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist[v88>.1&v88<.9], breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Congressional elections in 1988")
#dev.off()

#postscript ("hist88b1.ps", horizontal=T)
v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist[v88>.1&v88<.9&inc88==-1], ylim=c(0,50), breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Republican incumbents")
#dev.off()

#postscript ("hist88b2.ps", horizontal=T)
v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist[v88>.1&v88<.9&inc88==1], ylim=c(0,50), breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Democratic incumbents")
#dev.off()

#postscript ("hist88b3.ps", horizontal=T)
v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist[v88>.1&v88<.9&inc88==0], ylim=c(0,50), breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Open seats")
#dev.off()

#postscript ("cong.ps", horizontal=F)
par (pty="s", mar=c(5,5,4,1)+.1)
plot (0, 0, xlim=c(0,1), ylim=c(0,1), type="n",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
  cex.axis=2, cex.lab=2)
abline (0,1, lwd=.5)
j.v86 <- ifelse (contested86, v86, jitt (v86, .02))
j.v88 <- ifelse (contested88, v88, jitt (v88, .02))
points (j.v86[inc88==0], j.v88[inc88==0], pch=1, cex=2)
points (j.v86[inc88==1], j.v88[inc88==1], pch=16, cex=1)
points (j.v86[inc88==-1], j.v88[inc88==-1], pch=4, cex=1)
mtext ("Raw data", line=1, cex=2)
#dev.off()

#postscript ("congclean.ps", horizontal=F)
par (pty="s", mar=c(5,5,4,1)+.1)
plot (0, 0, xlim=c(0,1), ylim=c(0,1), type="n",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
  cex.axis=2, cex.lab=2)
abline (0,1, lwd=.5)
v86.adjusted <- ifelse (v86<.1, .25, ifelse (v86>.9, .75, v86))
vote.86 <- v86.adjusted[contested88]
vote.88 <- v88[contested88]
incumbency.88 <- inc88[contested88]
points (vote.86[incumbency.88==0], vote.88[incumbency.88==0], pch=1, cex=2)
points (vote.86[incumbency.88==1], vote.88[incumbency.88==1], pch=16, cex=1)
points (vote.86[incumbency.88==-1], vote.88[incumbency.88==-1], pch=4, cex=1)
mtext ("Adjusted data", line=1, cex=2)
#dev.off()

# regression predicting 1988 from 1986

fit.88 <- lm (vote.88 ~ vote.86 + incumbency.88)
print (fit.88)

print (lm (vote.88 ~ vote.86))

incparty.88 <- ifelse (vote.86>.5,1,-1)
incparty.88 <- ifelse (incumbency.88==1,1,incparty.88)
incparty.88 <- ifelse (incumbency.88==-1,-1,incparty.88)
print (lm (vote.88 ~ vote.86 + incparty.88 + incumbency.88))

# pull out the estimate of beta, its covariance matrix, and the estimated residual sd

n <- sum(!is.na(vote.88+vote.86+incumbency.88))
k <- 3
beta.hat <- coef(fit.88)
V.beta <- summary(fit.88,correlation=TRUE)$cov.unscaled
sd.hat <- sigma.hat(fit.88)

# picture of the model and data

#postscript ("congmodel.ps", horizontal=FALSE)
par (pty="s", mar=c(5,5,4,1)+.1)
plot (0, 0, xlim=c(0,1), ylim=c(0,1), type="n",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
  cex.axis=2, cex.lab=2)
points (vote.86[incumbency.88==0], vote.88[incumbency.88==0], pch=1, cex=2)
points (vote.86[incumbency.88==1], vote.88[incumbency.88==1], pch=16, cex=1)
points (vote.86[incumbency.88==-1], vote.88[incumbency.88==-1], pch=4, cex=1)

rng <- range (vote.86[incumbency.88==0], na.rm=TRUE)
lines (rng, beta.hat[1] + beta.hat[2]*rng)
text (min(rng), beta.hat[1] + beta.hat[2]*min(rng), "Inc = 0", adj=1, cex=1.5)
text (max(rng), beta.hat[1] + beta.hat[2]*max(rng), "Inc = 0", adj=0, cex=1.5)

rng <- range (vote.86[incumbency.88==1], na.rm=TRUE)
rng[1] <- .5
lines (rng, beta.hat[1] + beta.hat[3] + beta.hat[2]*rng)
text (max(rng), beta.hat[1] + beta.hat[3] + beta.hat[2]*max(rng),
  "Inc = +1", adj=0, cex=1.5)

rng <- range (vote.86[incumbency.88==-1], na.rm=TRUE)
lines (rng, beta.hat[1] - beta.hat[3] + beta.hat[2]*rng)
text (min(rng), beta.hat[1] - beta.hat[3] + beta.hat[2]*min(rng),
  "Inc = -1", adj=1, cex=1.5)

mtext ("Adjusted data with fitted model", line=1, cex=2)
#dev.off()

# residual plot

#postscript ("congresid.ps", horizontal=TRUE)
par (mar=c(5,5,4,1)+.1)
fitted <- fit.88$fitted
residual <- fit.88$residual
plot (fitted, residual, type="n",
  xlab="Fitted value from model", ylab="Observed - fitted",
  cex.axis=2, cex.lab=2)
abline (0, 0)
abline (sd.hat, 0, lty=2)
abline (-sd.hat, 0, lty=2)
points (fitted[incumbency.88==0], residual[incumbency.88==0], pch=1, cex=2)
points (fitted[incumbency.88==1], residual[incumbency.88==1], pch=16, cex=1)
points (fitted[incumbency.88==-1], residual[incumbency.88==-1], pch=4, cex=1)
mtext ("Residual plot", line=1, cex=2)
#dev.off()

# Crude estimate of how many Dems will win

n.new <- length(v88)
v88.adjusted <- ifelse (v88<.1, .25, ifelse (v88>.9, .75, v88))
v88.adjusted <- ifelse (is.na(v88), .5, v88.adjusted)
X.new <- cbind (rep(1,n.new), v88.adjusted, inc90)
y.hat.new <- as.vector (beta.hat %*% t(X.new))
dems.new.crude <- sum (y.hat.new[!is.na(v90)] > .5) +
  .5*sum (y.hat.new[!is.na(v90)]==.5)
print (dems.new.crude)

# Now, more appropriate simulation-based predictions for 1990 from 1988

# First, get simulations from the post distribution of beta and sigma
n.sims <- 1000
sigma <- rep (NA, n.sims)
beta <- array (NA, c(n.sims,k))
dimnames(beta) <- list (NULL, names(beta.hat))
for (s in 1:n.sims){
  sigma[s] <- sd.hat*sqrt((n-k)/rchisq(1,n-k))
  beta[s,] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
}

# Now, set up the simulations for all the districts
y.new <- array (NA, c(n.sims,n.new))
ok <- contested90 & !is.na(v90)
y.new[,ok] <- beta %*% t(X.new[ok,]) + rnorm (n.sims*sum(ok), 0, sigma)
y.new[,v90<.1] <- 0
y.new[,v90>.9] <- 1
y.new[,is.na(v90)] <- NA

# How many districts are contested?
print (sum(ok))

# Transform from continuous vote into Democratic or Republican seat
winner.new <- ifelse (y.new>.5, "Democrat", "Republican")
dems.new <- rep (NA, n.sims)

# For each simulation, sum over all the districts
for (s in 1:n.sims){
  dems.new[s] <- sum (winner.new[s,]=="Democrat", na.rm=TRUE)
}

# Finally:  our posterior mean and sd of how many districts the Dems will win
print (c(mean(dems.new), sqrt(var(dems.new))))

# Just for laffs, you can sum in the other direction and get Pr(Dem win) for each district
prob.dem.new <- rep (NA, n.new)
for (i in 1:n.new){
  prob.dem.new[i] <- mean (winner.new[,i]=="Democrat")
}

# plots of prediction errors

#postscript ("congpred.ps", horizontal=FALSE)
par (pty="s", mar=c(5,5,4,1)+.1)
predicted <- y.hat.new
actual <- v90
ok <- contested90 & !is.na(v88) & !is.na(v90)
num.cases <- sum(ok)
plot (predicted[ok], actual[ok], xlim=c(0,1), ylim=c(0,1),
  xlab="Predicted value from model", ylab="Actual value",
  cex.axis=2, cex.lab=2, pch=16, cex=.5)
abline (0, 1)
mtext ("Actual vs. predicted values", line=1, cex=2)
#dev.off()

#postscript ("congerr.ps", horizontal=TRUE)
par (mar=c(5,5,4,1)+.1)
plot (predicted[ok], (actual-predicted)[ok],
  xlab="Predicted value from model", ylab="Actual - predicted",
  cex.axis=2, cex.lab=2, pch=16, cex=.5)
abline (0, 0)
abline (sd.hat, 0, lty=2)
abline (-sd.hat, 0, lty=2)
mtext ("Prediction errors vs. predicted values", line=1, cex=2)
#dev.off()





# glm fits

v86.adjusted <- ifelse (is.na(v86), .5, v86.adjusted)
ok.88 <- !is.na(v88+v86.adjusted+inc88)
win.88 <- ifelse (v88>.5, 1, 0)
gfit.88 <- glm (win.88[ok.88] ~ v86.adjusted[ok.88] + inc88[ok.88],
  family=binomial(link="logit"))
print (gfit.88)

#postscript ("logitfita.ps", horizontal=T)
par (mar=c(5,5,4,1)+.1)
x <- seq(min(v86.adjusted,na.rm=T), max(v86.adjusted,na.rm=T), .001)
plot (0, 0, type="l", yaxs="i",
  xlab="Democratic vote share in 1986",
  ylab="Pr (Democrat wins in 1988)", xlim=range(x), ylim=c(0,1),
  cex.main=2.5, cex.axis=2.5, cex.lab=2.5) 
for (i in c(-1,0,1)){
  lines (x, invlogit(gfit.88$coef[1] + gfit.88$coef[2]*x + gfit.88$coef[3]*i))
}
text (.35, .6, "Inc=-1", cex=2)
text (.5, .4, "Inc=0", cex=2)
text (.6, .3, "Inc=+1", cex=2)
#dev.off()

#postscript ("logitfitb.ps", horizontal=T)
par (mar=c(5,5,4,1)+.1)
x <- seq(min(v86.adjusted,na.rm=T), max(v86.adjusted,na.rm=T), .001)
plot (0, 0, type="l", yaxs="i",
  xlab="Democratic vote share in 1986",
  ylab="Pr (Democrat wins in 1988)", xlim=range(x), ylim=c(0,1),
  cex.main=2.5, cex.axis=2.5, cex.lab=2.5) 
for (i in c(-1,0,1)){
  if (i==-1) x <- seq(min(v86.adjusted[inc88==-1],na.rm=T),.5,.001)
  if (i==0) x <- seq(min(v86.adjusted[inc88==0],na.rm=T),
    max(v86.adjusted[inc88==0],na.rm=T), .001)
  if (i==1) x <- seq(.5,max(v86.adjusted[inc88==1],na.rm=T),.001)
  lines (x, invlogit(gfit.88$coef[1] + gfit.88$coef[2]*x + gfit.88$coef[3]*i))
}
text (.5, .05, "Inc=-1", cex=2)
text (.5, .4, "Inc=0", cex=2)
text (.5, .95, "Inc=+1", cex=2)
#dev.off()

obs <- win.88[ok.88]
fitted <- invlogit(as.vector(cbind (rep(1,sum(ok.88)),
  v86.adjusted[ok.88], inc88[ok.88]) %*% gfit.88$coef))
residual <- obs - fitted
#postscript ("logitresidsa.ps", horizontal=T)
par (mar=c(5,5,4,1)+.1)
plot (fitted, residual, ylim=c(-1,1),
  xlab="Fitted probability of Democratic win in 1988",
  ylab="Observed - fitted",
  cex.axis=2, cex.lab=2, pch=16)
abline (0, 0)
mtext ("Residual plot", line=1, cex=2)
#dev.off()

#postscript ("logitresidsb.ps", horizontal=T)
par (mar=c(5,5,4,1)+.1)
binnedplot (fitted, residual)
#dev.off()

error.rate <- mean(fitted>0.5&obs==0 | fitted<.5&obs==1)
fitted <- fit.88$fitted
resid <- fit.88$resid
obs <- fitted+resid
linear.error.rate <- mean(fitted>0.5&obs<.50 | fitted<.5&obs>.5)
