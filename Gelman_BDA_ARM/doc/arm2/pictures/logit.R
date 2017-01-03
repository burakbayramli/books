postscript ("c:/books/multilevel/logitfigurea.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-6.5,6.5,.01)
plot (x, invlogit(x), type="l", xaxs="i", yaxs="i", xlab=expression(x),
  ylab=expression ({logit^{-1}}(x)), ylim=c(0,1),
  main=expression (paste ("y = ", {logit^{-1}}(x))),
  cex.main=2.5, cex.axis=2.5, cex.lab=2.5)
lines (c(-100,0,0), c(.5,.5,0), lty=2, lwd=.5, col="darkgray")
arrows (-.2,.5-.5/4,.8,.5+.5/4)
text (.5, .5, "slope = 1/4", cex=1.8, adj=0)
dev.off()

postscript ("c:/books/multilevel/logitfigureb.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq((1.4-6.5)/.33,(1.4+6.5)/.33,.01)
plot (x, invlogit(-1.40+.33*x), type="l", xaxs="i", yaxs="i", xlab=expression(x),
  ylab=expression (paste ({logit^{-1}}, "(-1.40 + .33 x)")), ylim=c(0,1),
  main=expression (paste ("y = ", {logit^{-1}}, "(-1.40 + 0.33 x)")),
  cex.main=2.5, cex.axis=2.5, cex.lab=2.5)
lines (c(-100,1.40/.33,1.40/.33), c(.5,.5,0), lty=2, lwd=.5, col="darkgray")
arrows (1.40/.33-.2/.33,.5-.5/4,1.40/.33+.8/.33,.5+.5/4)
text (1.40/.33+.5/.33, .5, "slope = 0.33/4", cex=1.8, adj=0)
dev.off()

postscript ("c:/books/multilevel/logitpdf.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-6.5,6.5,.01)
plot (x, exp(x)/(1+exp(x))^2, type="l", xaxs="i",yaxs="i", xlab=expression(x),
  ylab="", ylim=c(0,.28),
  main="Logistic probability density function", cex.main=2.5, cex.axis=2.5, cex.lab=2.5) 
dev.off()

postscript ("c:/books/multilevel/probitpdf.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-6.5,6.5,.01)
plot (x, exp(x)/(1+exp(x))^2, type="l", xaxs="i",yaxs="i", xlab=expression(x),
  ylab="", ylim=c(0,.28),
  main=expression (paste ("Normal (0, ", 1.6^2, ") probability density function")), cex.main=2.5, cex.axis=2.5, cex.lab=2.5)
dev.off()

postscript ("c:/books/multilevel/logitlatent.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-6.5,6.5,.01)
plot (x, exp(x+1.07)/(1+exp(x+1.07))^2, type="l", xaxs="i",yaxs="i", xlab=expression(x),
  ylab="", ylim=c(0,.28),
  main="Latent variable formulation of logit", cex.main=2.5, cex.axis=2.5, cex.lab=2.5)
x.pos <- x[x>=0]
y.pos <- (exp(x+1.07)/(1+exp(x+1.07))^2)[x>=0]
polygon (c(0,x.pos,max(x.pos)),c(0,y.pos,0), density=-1, col="gray")
dev.off()

postscript ("c:/books/multilevel/logitidentify.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-6.5,6.5,.01)
plot (0, 0, type="n", xaxs="i", xlab="x", ylab="y", xlim=range(x),
  ylim=c(0,1),  cex.main=2.5, cex.axis=2.5, cex.lab=2.5)
points (runif(20,-5,2), rep(0,20), pch=16, cex=2)
points (runif(20,2,5), rep(1,20), pch=16, cex=2)
lines (c(-6,2,2,6), c(0,0,1,1))
dev.off()


postscript ("c:/books/multilevel/logit.choice.a.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-10,12,.01)
plot (x, (1/1.6)*exp((x-.98)/1.6)/(1+exp((x-.98)/1.6))^2, type="l", xaxs="i",yaxs="i", xlab=expression(d), xlim=c(-10,12), ylim=c(0,.18),
  ylab="",
  main="Logistic population distribution for d", cex.main=2.5, cex.axis=2.5, cex.lab=2.5) 
dev.off()

postscript ("c:/books/multilevel/logit.choice.b.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-10,12,.01)
plot (x, 1-invlogit((x-.98)/1.6), type="l", xaxs="i",yaxs="i", xlab=expression(x),
  ylab="", xlim=c(-10,12), ylim=c(0,1),
  main="Logistic Pr(d>x) as a function of x", cex.main=2.5, cex.axis=2.5, cex.lab=2.5, lwd=.5, col="darkgray")
x <- seq(0,3.5,.01)
lines (x, 1-invlogit((x-.98)/1.6), lwd=4)
dev.off()

postscript ("c:/books/multilevel/probit.choice.a.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-10,12,.01)
plot (x, dnorm(x,.98,2.6), type="l", xaxs="i",yaxs="i", xlab=expression(d), xlim=c(-10,12), ylim=c(0,.18),
  ylab="",
  main=expression(paste("Normal (0, ", 2.6^2, ") population distribution for d")), cex.main=2.5, cex.axis=2.5, cex.lab=2.5) 
dev.off()

postscript ("c:/books/multilevel/probit.choice.b.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
x <- seq(-10,12,.01)
plot (x, 1-pnorm(x,.98,2.6), type="l", xaxs="i",yaxs="i", xlab=expression(x),
  ylab="", xlim=c(-10,12), ylim=c(0,1),
  main="Probit Pr(d>x) as a function of x", cex.main=2.5, cex.axis=2.5, cex.lab=2.5, lwd=.5, col="darkgray")
x <- seq(0,3.5,.01)
lines (x, 1-pnorm(x,.98,2.6), lwd=4)
dev.off()

postscript ("c:/books/multilevel/decisionplot.ps", horizontal=T)
par (mar=c(5,6,4,1)+.1)
plot (c(0,1), c(0,1), type="n", xlab=expression(paste("distance ", x[i], " to nearest safe well")), ylab=expression(paste("              current arsenic level, ", As[i])), bty="l", cex.main=2.5, cex.axis=2.5, cex.lab=2.5, xaxt="n", yaxt="n", xaxs="i", yaxs="i")
lines (c(0,1), c(.1,.8))
text (.15, .5, "Switch", adj=0, cex=2.5)
text (.3, .15, "Don't switch", adj=0, cex=2.5)
mtext (expression (b[i]/a[i]), 2, .6, at=.1, las=1, cex=2.5)
arrows (.5, .4, .7, .4+.2*.7)
text (.6, .45, expression (paste ("slope = ", c[i]/a[i])), adj=0, cex=2.5)
dev.off()


