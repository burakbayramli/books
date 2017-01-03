# simple scatterplots of correlated data

mu <- c(0,0)
sigma <- 1
rho <- 0.5
Sigma <- array (c(sigma^2, rho*sigma^2, rho*sigma^2, sigma^2), c(2,2))
n.sims <- 1000
x <- mvrnorm (n.sims, mu, Sigma, empirical=TRUE)

postscript ("c:/books/multilevel/correlation1a.ps", height=4.5, width=4.5)
par (pty="s", mar=c(4,4,3,2))
plot (x[,1], x[,2], xlim=range(x), ylim=range(x), xaxt="n", yaxt="n", xlab="x", ylab="y", mgp=c(1,0,0),
      main="principal component line", pch=20, cex.lab=1.4, cex.main=1.4, cex=.7)
abline (0,1,lwd=2)
dev.off()

postscript ("c:/books/multilevel/correlation1b.ps", height=4.5, width=4.5)
par (pty="s", mar=c(4,4,3,2))
plot (x[,1], x[,2], xlim=range(x), ylim=range(x), xaxt="n", yaxt="n", xlab="x", ylab="y", mgp=c(1,0,0),
      main="regression line of y on x", pch=20, cex.lab=1.4, cex.main=1.4, cex=.7)
abline (0, rho,lwd=2)
dev.off()
