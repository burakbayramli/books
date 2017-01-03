## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work

source("4.1_Linear transformations.R") # where data was cleaned

## Log transformation

log.earn <- log(earn)
earn.logmodel.1 <- lm(log.earn ~ height)
display(earn.logmodel.1)
 
 # Figure 4.3

sim.logmodel.1 <- sim (earn.logmodel.1)
beta.hat <- coef (earn.logmodel.1)

par (mar=c(6,6,4,2)+.1)
plot (height + runif(n,-.2,.2), log.earn, xlab="height", ylab="log (earnings)", pch=20, yaxt="n", mgp=c(4,2,0), col="gray10",
      main="Log regression, plotted on log scale")
axis (2, seq(6,12,2), mgp=c(4,1.1,0))
for (i in 1:20)
  curve (sim.logmodel.1$coef[i,1] + sim.logmodel.1$coef[i,2]*x, lwd=.5, col="gray", add=TRUE)
curve (beta.hat[1] + beta.hat[2]*x, add=TRUE, col="red")

par (mar=c(6,6,4,2)+.1)
plot (height + runif(n,-.2,.2), earn, xlab="height", ylab="earnings", pch=20, yaxt="n", mgp=c(4,2,0), col="gray10",
      main="Log regression, plotted on original scale")
axis (2, c(0,100000,200000), c("0","100000","200000"), mgp=c(4,1.1,0))
for (i in 1:20)
  curve (exp(sim.logmodel.1$coef[i,1] + sim.logmodel.1$coef[i,2]*x), lwd=.5, col="gray", add=TRUE)
curve (exp(beta.hat[1] + beta.hat[2]*x), add=TRUE, col="red")

## Log-base-10 transformation

log10.earn <- log10(earn)
earn.log10model <- lm(log10.earn ~ height)
display(earn.log10model)

## Log scale regression model

earn.logmodel.2 <- lm(log.earn ~ height + male)
display(earn.logmodel.2)

## Including interactions

earn.logmodel.3 <- lm(log.earn ~ height + male + height:male)
display(earn.logmodel.3)

## Linear transformations

z.height <- (height - mean(height))/sd(height)
earn.logmodel.4 <- lm(log.earn ~ z.height + male + z.height:male)
display(earn.logmodel.4)

## Log-log model

log.height <- log(height)
earn.logmodel.5 <- lm(log.earn ~ log.height + male)
display(earn.logmodel.5)

