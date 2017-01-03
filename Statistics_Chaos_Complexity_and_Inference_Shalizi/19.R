logistic.map <- function(x,r) {
  return(4*r*x*(1-x))
}


logistic.map.ts <- function(timelength,r,initial.cond=NULL) {
  x <-vector(mode="numeric",length=timelength)
  if(is.null(initial.cond)) {
    x[1] <-runif(1)
  } else {
    x[1] <-initial.cond
  }
  for (t in 2:timelength) {
    x[t] = logistic.map(x[t-1],r)
  }
  return(x)
}

logistic.noisy.ts <- function(timelength,r,initial.cond=NULL,noise.sd=0.1) {
	x <- logistic.map.ts(timelength,r,initial.cond)
	return(x+rnorm(timelength,0,noise.sd))
}

mean.logistic <- function(r,n=1e3,s=10) {
	mean(replicate(s,mean(logistic.map.ts(n,r))))
}

mean.logistic.plottable <- function(r,n=1e3,s=10) {
	sapply(r,mean.logistic,n=n,s=s)
}

# curve(mean.logistic.plottable)


var.logistic <- function(r,n=1e3,s=10) {
	mean(replicate(s,var(logistic.map.ts(n,r))))
}

var.logistic.plottable <- function(r,n=1e3,s=10) {
	sapply(r,var.logistic,n=n,s=s)
}

# curve(var.logistic.plottable,from=0,to=1,xlab="r",ylab="Variance of X")

msm.logistic.est <- function(x,S=10) {
	T <- length(x)
	mom <- vector(length=2)
	mom[1] <- mean(x)
	mom[2] <- var(x)
	moment.discrep <- function(r) {
		sims <- replicate(S,logistic.map.ts(T,r))
		momp <- vector(length=2)
		momp[1] <- mean(apply(sims,2,mean))
		momp[2] <- mean(apply(sims,2,var))
		return(sum((mom-momp)^2))
	}
	return(optimize(f=moment.discrep,lower=0,upper=1)$minimum)
}

ar.from.logmap <- function(r,T=1000,reps=10,order=2,coef=1) {
 	mean(replicate(reps,ar(logistic.noisy.ts(T,r),aic=FALSE,order.max=order)$ar[coef]))
}
 
ar.logmap.plottable <- function(r,...) {sapply(r,ar.from.logmap,...)}

logistic.map.II <- function(y,order=2,S=10) {
	T <- length(y)
	ar.fit <- function(x) {
		return(ar(x,aic=FALSE,order.max=order)$ar)
	}
	beta.data <- ar.fit(y)
	beta.discrep <- function(r) {
		beta.S <- mean(replicate(S,ar.fit(logistic.noisy.ts(T,r))))
		return(sum((beta.data - beta.S)^2))
	}
	return(optimize(beta.discrep,lower=0.75,upper=1))
}

mse.logistic.II <- function(T,r=0.9,reps=30,order=2,S=10) {
	II <- replicate(reps,logistic.map.II(logistic.noisy.ts(T,r),order=order,S=S)$minimum)
	II.error = II - r # Uses duplication
	return(mean(II.error^2))
} 