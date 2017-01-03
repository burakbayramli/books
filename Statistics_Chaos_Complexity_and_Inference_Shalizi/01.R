logistic.map <- function(x,r) {
	return(4*r*x*(1-x))
}

logistic.map.ts <- function (timelength,r,initial.cond=NULL) {
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

plot.logistic.map.trajectories <- function(timelength,num.traj,r) {
	plot(1:timelength,logistic.map.ts(timelength,r),lty=2,type="b",ylim=c(0,1),xlab="t",ylab="x(t)")
	i = 1
	while (i < num.traj) {
		i <- i+1
		x <- logistic.map.ts(timelength,r)
		lines(1:timelength,x,lty=2)
		points(1:timelength,x)
	}
}

logistic.map.evolution <- function(timesteps,r,x) {
	t=0
	while (t < timesteps) {
		x <- logistic.map(x,r)
		t <- t+1
	}
	return(x)
}

