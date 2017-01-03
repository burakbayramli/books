plot.decay.of.correlations.cos.sin = function(from=0,to=10,ts,...) {
	n=length(ts)
	rho = vector(mode="numeric",length=(to-from+1))
	for (t in from:to) {
		rho[t] = cor(cos(ts[1:(n-t)]),sin(ts[(1+t):n]))
	}
	plot(from:to,rho,xlim=c(from,to),xlab="lag",ylab="correlation",...)
}

rotation.map = function(x,angle) {
	x = x+angle
	x = (x %% (360))
	return(x)
}

rotation.map.evolution = function(timesteps,x,angle) {
	for (t in (1:timesteps)) {
		x = rotation.map(x,angle)
	}
	return(x)
}

rotation.map.ts = function(timesteps,x,angle) {
	x.ts = vector(mode="numeric",length=timesteps)
	for (t in (1:timesteps)) {
		x.ts[t] = x
		x = rotation.map(x,angle)
	}
	return(x.ts)
}

henon.map = function(x,a,b) {
	x.new=x
	x.new[1,] = a - x[1,]*x[1,] + b*x[2,]
	x.new[2,] = x[1,]
	x = x.new
	return(x.new)
}

henon.map.evolution = function(timesteps,x,a,b) {
	if (timesteps==0) {
		return(x)
	}
	for (t in (1:timesteps)) {
		x = henon.map(x,a,b)
	}
	return(x)
}

henon.plot = function(x,a,b,t,...) {
	x = henon.map.evolution(t,x,a,b)
	#title = expression(paste("t",==,t))
	plot(t(x),cex=0.1,xlab="x",ylab="y",main=substitute(t == time,list(time=t)),...)
}

henon.map.ts = function(timesteps,x,a,b) {
	x.ts = matrix(nrow=2,ncol=timesteps)
	x.ts[,1] = x[,1]
	for (t in (2:timesteps)) {
		x = henon.map(x,a,b)
		x.ts[,t] = x
	}
	return(x.ts)
}

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

plot.logistic.map.trajectories <- function(timelength,num.traj,r,lined=TRUE,cex=1,ic=NULL) {
	if(!is.null(ic) && (length(ic) != num.traj)) stop("number of initial conditions must match number of trajectories")
	plot(NULL,NULL,xlim=c(0,timelength),ylim=c(0,1),xlab="t",ylab="x(t)")
	i = 0
	while (i < num.traj) {
		i <- i+1
		if (is.null(ic)) {
			x <- logistic.map.ts(timelength,r)
		} else {
			x <- logistic.map.ts(timelength,r,initial.cond=ic[i])
		}
		if (lined==TRUE) {
			lines(1:timelength,x,lty=2)
		}
		points(1:timelength,x,cex=cex)
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

plot.logistic.map.evolution = function(timesteps,r,x,n=101,main=substitute(t == time,list(time=timesteps)),...) {
	if (timesteps > 0) {
		for (t in 1:timesteps) {
			x = logistic.map(x,r)
		}
	}
	hist(x,xlab="x",freq=FALSE,ylab="probability density",main=main,n=n,...)
}

qqnorm.logistic.map.cosine.timeaverages <- function(timesteps,num.traj,r,main=substitute(t == time,list(time=timesteps-1)),...) {
	x.avg = vector(mode="numeric",length=num.traj)
	for (i in 1:num.traj) {
		x = cos(2*pi*logistic.map.ts(timesteps,r))
		x.avg[i] = sum(x)/timesteps
	}
	x.avg = sqrt(timesteps)*x.avg
	qqnorm(x.avg,main=main,...)
}

