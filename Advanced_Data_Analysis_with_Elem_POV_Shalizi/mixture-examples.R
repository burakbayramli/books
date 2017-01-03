# R code for examples in Lecture 20

# Data preparation
snoqualmie <- read.csv("http://www.stat.cmu.edu/~cshalizi/402/lectures/16-glm-practicals/snoqualmie.csv",header=FALSE)
snoqualmie.vector <- na.omit(unlist(snoqualmie))
snoq <- snoqualmie.vector[snoqualmie.vector > 0]


### Figure 1
plot(hist(snoq,breaks=101),col="grey",border="grey",freq=FALSE,
     xlab="Precipitation (1/100 inch)",main="Precipitation in Snoqualmie Falls")
lines(density(snoq),lty=2)


# Two-component Gaussian mixture
library(mixtools)
snoq.k2 <- normalmixEM(snoq,k=2,maxit=100,epsilon=0.01)

# Function to add Gaussian mixture components, vertically scaled, to the
# current plot
  # Presumes the mixture object has the structure used by mixtools
plot.normal.components <- function(mixture,component.number,...) {
  curve(mixture$lambda[component.number] *
        dnorm(x,mean=mixture$mu[component.number],
        sd=mixture$sigma[component.number]), add=TRUE, ...)
}

### Figure 2
plot(hist(snoq,breaks=101),col="grey",border="grey",freq=FALSE,
     xlab="Precipitation (1/100 inch)",main="Precipitation in Snoqualmie Falls")
lines(density(snoq),lty=2)
sapply(1:2,plot.normal.components,mixture=snoq.k2)


# Function to calculate the cumulative distribution function of a Gaussian
# mixture model
  # Presumes the mixture object has the structure used by mixtools
  # Doesn't implement some of the usual options for CDF functions in R, like
  # returning the log probability, or the upper tail probability
pnormmix <- function(x,mixture) {
  lambda <- mixture$lambda
  k <- length(lambda)
  pnorm.from.mix <- function(x,component) {
    lambda[component]*pnorm(x,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  pnorms <- sapply(1:k,pnorm.from.mix,x=x)
  return(rowSums(pnorms))
}


#### Figure 3
# Distinct values in the data
distinct.snoq <- sort(unique(snoq))
# Theoretical CDF evaluated at each distinct value
tcdfs <- pnormmix(distinct.snoq,mixture=snoq.k2)
# Empirical CDF evaluated at each distinct value
  # ecdf(snoq) returns an object which is a _function_, suitable for application
  # to new vectors
ecdfs <- ecdf(snoq)(distinct.snoq)
# Plot them against each other
plot(tcdfs,ecdfs,xlab="Theoretical CDF",ylab="Empirical CDF",xlim=c(0,1),
     ylim=c(0,1))
# Main diagonal for visual reference
abline(0,1)


# Probability density function for a Gaussian mixture
  # Presumes the mixture object has the structure used by mixtools
dnormalmix <- function(x,mixture,log=FALSE) {
  lambda <- mixture$lambda
  k <- length(lambda)
  # Calculate share of likelihood for all data for one component
  like.component <- function(x,component) {
    lambda[component]*dnorm(x,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  # Create array with likelihood shares from all components over all data
  likes <- sapply(1:k,like.component,x=x)
  # Add up contributions from components
  d <- rowSums(likes)
  if (log) {
    d <- log(d)
  }
  return(d)
}

# Log likelihood function for a Gaussian mixture, potentially on new data
loglike.normalmix <- function(x,mixture) {
  loglike <- dnormalmix(x,mixture,log=TRUE)
  return(sum(loglike))
}

# Evaluate various numbers of Gaussian components by data-set splitting
# (i.e., very crude cross-validation)
n <- length(snoq)
data.points <- 1:n
data.points <- sample(data.points) # Permute randomly
train <- data.points[1:floor(n/2)] # First random half is training
test <- data.points[-(1:floor(n/2))] # 2nd random half is testing
candidate.component.numbers <- 2:10
loglikes <- vector(length=1+length(candidate.component.numbers))
# k=1 needs special handling
mu<-mean(snoq[train]) # MLE of mean
sigma <- sd(snoq[train])*sqrt((n-1)/n) # MLE of standard deviation
loglikes[1] <- sum(dnorm(snoq[test],mu,sigma,log=TRUE))
for (k in candidate.component.numbers) {
  mixture <- normalmixEM(snoq[train],k=k,maxit=400,epsilon=1e-2)
  loglikes[k] <- loglike.normalmix(snoq[test],mixture=mixture)
}

### Figure 4
plot(x=1:10, y=loglikes,xlab="Number of mixture components",
     ylab="Log-likelihood on testing data")

### Figure 5
snoq.k9 <- normalmixEM(snoq,k=9,maxit=400,epsilon=1e-2)
plot(hist(snoq,breaks=101),col="grey",border="grey",freq=FALSE,
     xlab="Precipitation (1/100 inch)",main="Precipitation in Snoqualmie Falls")
lines(density(snoq),lty=2)
sapply(1:9,plot.normal.components,mixture=snoq.k9)

### Figure 6
  # Assigments for distinct.snoq and ecdfs are redundant if you've already
  # made Figure 3
distinct.snoq <- sort(unique(snoq))
tcdfs <- pnormmix(distinct.snoq,mixture=snoq.k9)
ecdfs <- ecdf(snoq)(distinct.snoq)
plot(tcdfs,ecdfs,xlab="Theoretical CDF",ylab="Empirical CDF",xlim=c(0,1),
     ylim=c(0,1))
abline(0,1)

### Figure 7
plot(0,xlim=range(snoq.k9$mu),ylim=range(snoq.k9$sigma),type="n",
     xlab="Component mean", ylab="Component standard deviation")
points(x=snoq.k9$mu,y=snoq.k9$sigma,pch=as.character(1:9),
       cex=sqrt(0.5+5*snoq.k9$lambda))

### Figure 8
plot(density(snoq),lty=2,ylim=c(0,0.04),
     main=paste("Comparison of density estimates\n",
                "Kernel vs. Gaussian mixture"),
     xlab="Precipitation (1/100 inch)")
curve(dnormalmix(x,snoq.k9),add=TRUE)


# Do the classes of the Gaussian mixture make sense as annual weather patterns?
# Most probable class for each day:
day.classes <- apply(snoq.k9$posterior,1,which.max)
# Make a copy of the original, structured data set to edit
snoqualmie.classes <- snoqualmie
# Figure out which days had precipitation
wet.days <- (snoqualmie > 0) & !(is.na(snoqualmie))
# Replace actual precipitation amounts with classes
snoqualmie.classes[wet.days] <- day.classes
# Problem: the number of the classes doesn't correspond to e.g. amount of
# precipitation expected.  Solution: label by expected precipitation, not by
# class number.
snoqualmie.classes[wet.days] <- snoq.k9$mu[day.classes]

### Figure 9
plot(0,xlim=c(1,366),ylim=range(snoq.k9$mu),type="n",xaxt="n",
     xlab="Day of year",ylab="Expected precipiation (1/100 inch)")
axis(1,at=1+(0:11)*30)
for (year in 1:nrow(snoqualmie.classes)) {
  points(1:366,snoqualmie.classes[year,],pch=16,cex=0.2)
}

# Next line is currently (5 April 2011) used to invoke a bug-patch kindly
# provided by Dr. Derek Young; the patch will be incorporated in the next
# update to mixtools, so should not be needed after April 2011
source("http://www.stat.cmu.edu/~cshalizi/402/lectures/20-mixture-examples/bootcomp.R")
snoq.boot <- boot.comp(snoq,max.comp=10,mix.type="normalmix",
                       maxit=400,epsilon=1e-2)
# Running this takes about 5 minutes

### Figure 10
# automatically produced as a side-effect of running boot.comp()

### Figure 11
library(mvtnorm)
x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
     z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),mean=mu,sigma=sigma)
  }
}
contour(x.points,y.points,z)
# Using expand.grid, as in Lecture 6, would be more elegant than this double
# for loop

