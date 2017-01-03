chickens <- read.table ("chickens.dat", header=T)
attach.all (chickens)
diff <- mean.control - mean.treated
se.diff <- sqrt (se.treated^2 + se.control^2)

postscript ("c:/books/multilevel/blackman1.ps", horizontal=T, height=4, width=4.5)
plot (freq, diff, xlab="Frequency of magnetic field (Hz)",
      ylab="Est. treatment effect", type="n",
      main=expression("Estimates with statistical significance"), mgp=c(1.5,.5,0))
w <- 2.5
freq[freq==165] <- 165+c(-w,w)
freq[freq==180] <- 180+c(-w,w)
freq[freq==405] <- 405+c(-w,w)
pvalue <- 2*pt(-diff/se.diff,n.treated+n.control-2)
for (i in 1:nrow(chickens))
  polygon (freq[i] + c(-w,-w,w,w), c(diff[i],0,0,diff[i]),
           col=ifelse(pvalue[i]<.05,"black",0), lwd=.5)
dev.off()

postscript ("c:/books/multilevel/blackman2.ps", horizontal=T, height=4, width=4.5)
plot (range(freq), range(c(diff+se.diff,diff-se.diff)),
      xlab="Frequency of magnetic field (Hz)",
      ylab="Est. treatment effect", type="n",
      main=expression("Estimates" %+-% "standard errors"), mgp=c(1.5,.5,0))
abline (0,0,lty=2,lwd=.5)
points (freq, diff, pch=20)
for (i in 1:nrow(chickens))
  lines (rep(freq[i],2), diff[i]+se.diff[i]*c(-1,1), lwd=.5)
dev.off()

if (0){
plot (range(freq), range(c(mean.control+se.control,mean.control-se.control))-1,
      xlab="Frequency of magnetic field (Hz)",
      ylab="Estimated treatment effect", type="n",
      main=expression("Estimates" %+-% "standard errors"))
abline (0,0,lty=2,lwd=.5)
points (freq, mean.control-1, pch=20)
for (i in 1:nrow(chickens))
  lines (rep(freq[i],2), mean.control[i]+se.control[i]*c(-1,1)-1, lwd=.5)
}


y <- diff
J <- length(diff)
sigma.y <- se.diff
schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
schools.parameters <- c("theta", "mu.theta", "sigma.theta", "e.theta", "y.rep")
# run in winbugs14
schools.sim <- bugs (schools.data, schools.inits, schools.parameters, "schools.bug", n.chains=3, n.iter=1000, version=1.4)
lambda.theta <- 1 - var (apply (e.theta, 2, mean)) / mean (apply (e.theta, 1, var))
print (lambda.theta)

theta.hat <- apply(theta,2,mean)
se.theta <- apply(theta,2,sd)
postscript ("c:/books/multilevel/blackman3.ps", horizontal=T, height=4, width=4.5)
plot (range(freq), range(c(diff+se.diff,diff-se.diff)),
      xlab="Frequency of magnetic field (Hz)",
      ylab="Est. treatment effect", type="n",
      main=expression("Multilevel estimates" %+-% "standard errors"), mgp=c(1.5,.5,0))
abline (0,0,lty=2,lwd=.5)
points (freq, theta.hat, pch=20)
for (i in 1:nrow(chickens))
  lines (rep(freq[i],2), theta.hat[i]+se.theta[i]*c(-1,1), lwd=.5)
dev.off()


