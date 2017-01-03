### data ~ t(3)
loglik.fun <- function(x,theta){
     n   <- length(x)
     nth <- length(theta)
     loglik <- rep(0,nth)
     for(i in 1:n){
          loglik <- loglik + log(dt(x[i]-theta,3))
          }
     loglik <- loglik - max(loglik)
     loglik
     }


n <- 2
x <- rt(n,3)
a <- max(abs(x))
m <- 10000
theta <- seq(-6,6,length=m)
loglik <- loglik.fun(x,theta)
lik <-  exp(loglik)
post <- exp(loglik)
delta <- theta[2]-theta[1]
post <- post/(delta*sum(post))
normalpost <- dnorm(theta,mean(x),1/sqrt(n))
b <- max(c(post,normalpost))

postscript("importance.sampling.ps")
par(mfrow=c(1,1),pty="s")
plot(theta,post,type="l",lwd=3,xlab="theta",ylab="f(theta | data)",ylim=c(0,b))
lines(theta,normalpost,lwd=1,lty=2,col=2)
postmean <- delta*sum(theta*post)
print(postmean)

### N(xbar,1/n) importance sampler
nsim <- 10000
THETA <- rnorm(nsim, mean(x),1/sqrt(n))
lik <- exp(loglik.fun(x,THETA))
g   <-  dnorm(THETA,mean(x),1/sqrt(n))
estimate <- mean(THETA*lik/g)/mean(lik/g)
print(estimate)

### Cauchy importance sampler
THETA <- rcauchy(nsim, mean(x))
lik <- exp(loglik.fun(x,THETA))
g   <-  dcauchy(THETA,mean(x))
estimate <- mean(THETA*lik/g)/mean(lik/g)
print(estimate)

dev.off()

