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

plot.logistic.map.bifurcations <- function(from=0,to=1,n=201,plotted.points=1000,transients=10000,ylim=c(0,1)) {
	r.values = seq(from=from,to=to,length.out=n)
	total.time = transients+plotted.points
	plot(NULL,NULL,xlab="r",ylab="x",xlim=c(from,to),ylim=ylim,main="Bifurcation Diagram for Logistic Map")
	for (r in r.values) {
		x = logistic.map.ts(total.time,r)
		x = x[(transients+1):total.time]
		points(rep(r,times=plotted.points),x,cex=0.01)
	}
}

plot.logistic.map.trajectories <- function(timelength,num.traj,r,lined=TRUE,cex=1) {
	plot(NULL,NULL,xlim=c(0,timelength),ylim=c(0,1),xlab="t",ylab="x(t)")
	i = 0
	while (i < num.traj) {
		i <- i+1
		x <- logistic.map.ts(timelength,r)
		if (lined==TRUE) {
			lines(1:timelength,x,lty=2)
		}
		points(1:timelength,x,cex=cex)
	}
}

logistic.threestep <- function(x,r) {
	logistic.map(logistic.map(logistic.map(x,r),r),r)
}

plot.logistic.map.timeaverages <- function(timelength,num.traj,r,lined=TRUE,cex=1) {
	plot(NULL,NULL,xlim=c(0,timelength),ylim=c(0,1),xlab="t",ylab="Time average of x(t)")
	i = 0
	while (i < num.traj) {
		i <- i+1
		x <- logistic.map.ts(timelength,r)
		x.avg = cumsum(x)/(1:timelength)
		if (lined==TRUE) {
			lines(1:timelength,x.avg,lty=2)
		}
		points(1:timelength,x.avg,cex=cex)
	}
}

plot.logistic.map.cosine.timeaverages <- function(timelength,num.traj,r,lined=TRUE,cex=1) {
	plot(NULL,NULL,xlim=c(0,timelength),ylim=c(-1,1),xlab="t",ylab="Time average of cos(2pi x(t))")
	i = 0
	while (i < num.traj) {
		i <- i+1
		x <- cos(2*pi*logistic.map.ts(timelength,r))
		x.avg = cumsum(x)/(1:timelength)
		if (lined==TRUE) {
			lines(1:timelength,x.avg,lty=2)
		}
		points(1:timelength,x.avg,cex=cex)
	}
}

plot.little.line = function(center,width,height,...) {
	lines(c(center-width,center+width),c(height,height),...)
}

logistic.map.fates = function(iterations,n=1000,from=0,to=1,r=1,...) {
	x = seq(from=from,to=to,length.out=n)
	x.ic = x
	plot(NULL,NULL,xlim=c(from,to),ylim=c(0,iterations),xlab="x",ylab="t")
	iterate = 0
	while(iterate <= iterations) {
		blacks = x.ic[x <= 0.5]
		reds = x.ic[x > 0.5]
		num.blacks = length(blacks)
		num.reds = length(reds)
		sapply(blacks, plot.little.line, width=1/(2*n),height=iterate,col="black",...)
		sapply(reds, plot.little.line, width=1/(2*n),height=iterate,col="red",...)
		iterate = iterate+1
		x = logistic.map(x,r)
	}
}