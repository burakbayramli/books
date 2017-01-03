# revised 05/05/03

invisible(setwd("c:/Ideal Point Innov"))


postscript("outliers.ps", height=6, horizontal=T)

# plot logit function
par(mfrow=c(1,2))
# logit plot w/o outliers
x <- seq(-10, 10, .01)
y <- invlogit(x)
plot (x, y, xlim=c(-10,10), ylim=c(0,1), axes=FALSE, xlab="x", ylab="Pr(y=1)", cex.lab=1.2, main="Logit without outlier", type="l")
par("new"=TRUE)
x <- seq(3, 8, .5)
y <- rep(1,length(x))
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x")
par("new"=TRUE)
x <- seq(-8, -3, .5)
y <- rep(0,length(x))
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x")

# horizontal line (y=0)
x0<-c(-10,10)
y0<-c(0,0)
lines(x0,y0)

# arrows (x=0)
arrows(-10, 0, 10, 0, length=0.1)
arrows(10, 0, -10, 0, length=0.1)

# horizontal line (y=1)
x1<-c(-10,10)
y1<-c(1,1)
lines(x1,y1, lty=8)

# vertical line (x=0)
xv<-c(0,0)
yv<-c(0,1.025)
lines(xv,yv)
arrows(0,0, 0, 1.025, length=0.1)

# text
mtext("1", at=0)
mtext("0", side=1, at=0)

# logit plot w/outliers
x <- seq(-10, 10, .01)
y <- invlogit(x)
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="x", ylab="Pr(y=1)", cex.lab=1.2, axes=FALSE, main="Logit with outlier", type="l")
par("new"=TRUE)
x <- seq(3, 8, .5)
y <- rep(1,length(x))
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x")
par("new"=TRUE)
x <- seq(-8, -3, .5)
y <- rep(0,length(x))
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x")
par("new"=TRUE)
x <- seq(-8, -8, 1)
y <- rep(1,length(x))
plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x")
#plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x", col="red")
#par("new"=TRUE)
#x <- seq(5, 6, 1)
#y <- rep(0,length(x))
#plot (x, y, xlim=c(-10,10), ylim=c(0,1), xlab="", ylab="", axes=FALSE, pch="x", col="red")

# horizontal line (y=0)
x0<-c(-10,10)
y0<-c(0,0)
lines(x0,y0)

# arrows (x=0)
arrows(-10, 0, 10, 0, length=0.1)
arrows(10, 0, -10, 0, length=0.1)

# horizontal line (y=1)
x1<-c(-10,10)
y1<-c(1,1)
lines(x1,y1, lty=8)

# vertical line (x=0)
xv<-c(0,0)
yv<-c(0,1.025)
lines(xv,yv)
arrows(0,0, 0, 1.025, length=0.1)

# text
mtext("1", at=0)
mtext("0", side=1, at=0)


dev.off()

#savePlot(filename="outliers", type=c("ps"), device=dev.cur()) 



postscript("logit.error.ps", height=7, horizontal=T)

# logit function with epsilons
par(mfrow=c(1,1))
x <- seq(-10, 10, .01)
y <- invlogit(x)
plot (x, y, xlim=c(-10,10), ylim=c(-.25,1.25), xlab="", ylab="Pr(y=1)",
cex.lab=1.2, axes=FALSE, main="", type="l")

# horizontal line (y=0)
x0<-c(-10,10)
y0<-c(-.05,-.05)
lines(x0,y0)

# arrows (x=0)
# arrows(-10, 0, 10, 0, length=0.1)
# arrows(10, 0, -10, 0, length=0.1)

# horizontal line (y=1)
x1<-c(-10,10)
y1<-c(1.05,1.05)
lines(x1,y1)

# vertical line (x=0)
# xv<-c(0,0)
# yv<-c(-.05,1.085)
# lines(xv,yv)
# arrows(0,0, 0, 1.085, length=0.1)

# margin text
# mtext("1", at=0)
# mtext("0", side=1, at=0)

# text
text(.5, 1, expression(epsilon[1]), cex=1.7)
text(-.5, 0, expression(epsilon[0]), cex=1.7)

# error line 1
x1<-c(1,5)
y1<-c(1,1)
lines(x1,y1, lty=8)

# error line 0
x1<-c(-5, -1)
y1<-c(0,0)
lines(x1,y1, lty=8)


dev.off()

#savePlot(filename="logit.error.ps", type=c("ps"), device=dev.cur()) 
