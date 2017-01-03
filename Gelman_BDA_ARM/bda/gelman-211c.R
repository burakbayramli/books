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
thetas <- sample (theta, 1000, step*dens.norm,replace=TRUE)
hist (thetas, xlab="theta", yaxt="n", breaks=seq(0,1,.05), cex=2)
  
print (thetas)
y6 <- rcauchy (length(thetas), thetas, 1)
hist (y6, xlab="new observation", yaxt="n", nclass=100, cex=2)
