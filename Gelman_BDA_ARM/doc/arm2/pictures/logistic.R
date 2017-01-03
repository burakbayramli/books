dlogit <- function (a) {1/(exp(a)+exp(-a))^2}

vline <- function (x, shift) {lines (rep(x,2), c(0,dlogit(x-shift)))}

lplot <- function (shift){
  par (mar=c(8,4,4,4))
  curve (dlogit(x-shift), -4.1, 6.1, xaxs="i", yaxs="i", xlab=expression(paste("z = X", beta, "+", epsilon)), yaxt="n", ylab="", ylim=c(0,.28), bty="n",
         main=substitute(paste ("X", beta)==bta, list(bta=shift)),
         cex.lab=4, cex.axis=3, cex.main=4, mgp=c(7,3,0))
  vline (0,shift)
  vline (.8,shift)
  vline(1.8,shift)
}

postscript ("c:/books/multilevel/logistic1a.ps", height=7, horizontal=T)
lplot(-.7)
dev.off ()
postscript ("c:/books/multilevel/logistic1b.ps", height=7, horizontal=T)
lplot(1.2)
dev.off ()
postscript ("c:/books/multilevel/logistic1c.ps", height=7, horizontal=T)
lplot(2.8)
dev.off ()

n <- 50
x <- runif (n,-9,9)
y <- rbinom (n, 1, invlogit(x))
y[rank(x)==5] <- 1
y[rank(x)==5] <- 0
plot (x,y)

glm.1 <- glm (y ~ x, family="binomial")
display(glm.1)

glm.2 <- glm (y ~ x, family="binomial", subset=(rank(x)!=5))
display(glm.2)

#thresh <- 10
#data <- list ("n", "y", "x", "thresh")
#inits <- function (){
#  list (a=rnorm(1), b=rnorm(1))
#}
#params <- c("a", "b")
#model.1 <- bugs (data, inits, params, "robustlogit1.txt", n.chains=3, n.iter=100, digits=2)

data <- list ("n", "y", "x", "e")
inits <- function (){
  list (a=rnorm(1), b=rnorm(1), is.noise=rbinom(n,1,.5))#, e=runif(1,0,.1))
}
params <- c("a", "b", "is.noise")

e <- .001
model2.001 <- bugs (data, inits, params, "robustlogit2.txt", n.chains=3, n.iter=5000, digits=2)
e <- .01
model2.01 <- bugs (data, inits, params, "robustlogit2.txt", n.chains=3, n.iter=5000, digits=2)
e <- .1
model2.1 <- bugs (data, inits, params, "robustlogit2.txt", n.chains=3, n.iter=5000, digits=2)


df <- 8
z.lo <- ifelse (y==0, -100, 0)
z.hi <- ifelse (y==0, 0, 100)
data <- list ("n", "z.lo", "z.hi", "x", "df")
inits <- function (){
  list (a=rnorm(1), b=rnorm(1), z=runif(10)*sign(y-.5))
}
params <- c("a", "b")
model3.1 <- bugs (data, inits, params, "robustlogit3.txt", n.chains=3, n.iter=1000)

df <- 4
model3.2 <- bugs (data, inits, params, "robustlogit3.txt", n.chains=3, n.iter=5000)

data <- list ("n", "z.lo", "z.hi", "x")
inits <- function (){
  list (a=rnorm(1), b=rnorm(1), z=runif(10)*sign(y-.5), df.inv=runif(1,0,.5))
}
params <- c("a", "b", "df")
model4 <- bugs (data, inits, params, "robustlogit4.txt", n.chains=3, n.iter=10000)

#par (mfrow=c(1,1))
#plot (x,y)
#curve (invlogit(model2.001$median$a+model2.001$median$b*x), add=T, col="red", lwd=.5)
#curve (invlogit(model2.01$median$a+model2.01$median$b*x), add=T, col="green", lwd=.5)
#curve (invlogit(model2.1$median$a+model2.1$median$b*x), add=T, col="blue", lwd=.5)
#curve (invlogit(glm.1$coef[1]+glm.1$coef[2]*x), add=T, lwd=.5, lty=2)
#curve (pt(model3.1$median$a+model3.2$median$b*x,8), add=T, col="purple", lwd=.5, lty=2)
#curve (pt(model3.2$median$a+model3.2$median$b*x,8), add=T, col="purple", lwd=.5)
#curve (invlogit(glm.2$coef[1]+glm.2$coef[2]*x), add=T, lwd=.5, col="yellow",lwd=.5)

postscript ("c:/books/multilevel/logistic2a.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (x, y, yaxt="n", cex.lab=2.5, cex.axis=2.5, cex=2.5, cex.main=2.5, main="Contaminated data", mgp=c(4,2,0))
axis (2, c(0,1), cex.axis=2.5)
#curve (pt(model3.2$median$a+model3.2$median$b*x,8), add=T, lwd=1)
attach.all (model4$sims.list)
a.00 <- median (a*sqrt(df/(df-2)))
b.00 <- median (b*sqrt(df/(df-2)))
curve (pt (a.00 + b.00*x, median(df)), add=T, lwd=1)
curve (invlogit(glm.1$coef[1]+glm.1$coef[2]*x), add=T, lwd=.5, lty=2)
#curve (invlogit(glm.2$coef[1]+glm.2$coef[2]*x), add=T, lwd=.5, col="gray",lwd=.5)
legend (0, .3, c("fitted logistic regression", "fitted robit regression"), lty=c(2,1), cex=1.8)
dev.off()

postscript ("c:/books/multilevel/logistic2b.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (x, y, yaxt="n", cex.lab=2.5, cex.axis=2.5, cex=2.5, cex.main=2.5, main="Data from a logistic regression", mgp=c(4,2,0))
axis (2, c(0,1), cex.axis=2.5)
#curve (pt(model3.2$median$a+model3.2$median$b*x,8), add=T, lwd=1)
attach.all (model4$sims.list)
a.00 <- median (a*sqrt(df/(df-2)))
b.00 <- median (b*sqrt(df/(df-2)))
curve (pt (a.00 + b.00*x, median(df)), add=T, lwd=1)
curve (invlogit(glm.1$coef[1]+glm.1$coef[2]*x), add=T, lwd=.5, lty=2)
#curve (invlogit(glm.2$coef[1]+glm.2$coef[2]*x), add=T, lwd=.5, col="gray",lwd=.5)
legend (0, .3, c("fitted logistic regression", "fitted robit regression"), lty=c(2,1), cex=1.8)
dev.off()
