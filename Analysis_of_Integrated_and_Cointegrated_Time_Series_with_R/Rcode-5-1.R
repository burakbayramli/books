set.seed(123456)
e <- rnorm(500)
# trend
trd <- 1:500
S <- c(rep(0, 249), rep(1, 251))
# random walk with drift
y1 <- 0.1*trd + cumsum(e)
# random walk with drift and shift
y2 <- 0.1*trd + 10*S + cumsum(e)
# plotting
par(mar=rep(5,4))
plot.ts(y1, lty=1, ylab='', xlab='')
lines(y2, lty=2)
legend(10, 50, legend=c('rw with drift', 'rw with drift and pulse'), lty=c(1, 2))
