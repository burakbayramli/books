
# CI for continuous data

y <- c(35,34,38,35,37)
n <- length(y)
estimate <- mean(y)
se <- sd(y)/sqrt(n)
int.50 <- estimate + qt(c(.25,.75),n-1)*se
int.95 <- estimate + qt(c(.025,.975),n-1)*se

# CI for proportions

y <- 700
n <- 1000
estimate <- y/n
se <- sqrt (estimate*(1-estimate)/n)
int.95 <- estimate + qnorm(c(.025,.975))*se

# CI for discrete data

y <- rep (c(0,1,2,3,4), c(600,300,50,30,20))
n <- length(y)
estimate <- mean(y)
se <- sd(y)/sqrt(n)
int.50 <- estimate + qt(c(.25,.75),n-1)*se
int.95 <- estimate + qt(c(.025,.975),n-1)*se

# Plot Figure 2.3

par (mar=c(5,5,4,2)+.1)
polls <- matrix (scan("polls.dat"), ncol=5, byrow=TRUE)
support <- polls[,3]/(polls[,3]+polls[,4])
year <-  polls[,1] + (polls[,2]-6)/12
plot (year, support*100, xlab="Year", ylim=c(min(100*support)-1, max(100*support)+1),
      ylab="Percentage support for the death penalty", cex=1.1, cex.main=1.2,
      cex.axis=1.1, cex.lab=1.1, pch=20)
for (i in 1:nrow(polls))
  lines (rep(year[i],2), 100*(support[i]+c(-1,1)*sqrt(support[i]*(1-support[i])/1000)))

# Weighted averages

w.avg <- sum(N*p)/sum(N)
se.w.av <- sqrt (sum ((N*se/sum(N))^2))
int.95 <- w.avg + c(-2,2)*se.w.avg

# CI using simulations

n.men <- 500
p.hat.men <- 0.75
se.men <- sqrt (p.hat.men*(1-p.hat.men)/n.men)

n.women <- 500
p.hat.women <- 0.65
se.women <- sqrt (p.hat.women*(1-p.hat.women)/n.women)

n.sims <- 10000
p.men <- rnorm (n.sims, p.hat.men, se.men)
p.women <- rnorm (n.sims, p.hat.women, se.women)
ratio <- p.men/p.women
int.95 <- quantile (ratio, c(.025,.975))


