dens <- function (y, th){
  dens0 <- NULL
  for (i in 1:length(th))
    dens0 <- c(dens0, prod (dcauchy (y, th[i], 1)))
  dens0}
y <- c(-2, -1, 0, 1.5, 2.5)
step <- .01
theta <- seq(step/2, 1-step/2, step)
dens.unnorm <- dens(y,theta)
dens.norm <- dens.unnorm/(step*sum(dens.unnorm))
plot (theta, dens.norm, ylim=c(0,1.1*max(dens.norm)),
  type="l", xlab="theta", ylab="normalized density",
  xaxs="i", yaxs="i", cex=2)

  
print (step*dens.norm)
print (theta)
thetas <- sample (theta, 1000, step*dens.norm,replace=TRUE)
print (thetas)
hist (thetas, xlab="theta", yaxt="n", breaks=seq(0,1,.05), cex=2)
  