# Logistic regression for arsenic data

wells <- read.table ("wells.dat", header=TRUE)

attach.all (wells)

# Logistic regression of switching on distance to nearest safe well

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.1)

# Redefine distance in 100-meter units and fit the model again

dist100 <- dist/100
fit.2 <- glm (switch ~ dist100, family=binomial(link="logit"))
display (fit.2)

# histogram of distances

#postscript ("c:/books/multilevel/arsenic.distances.bnew.ps", height=3, width=4, horizontal=TRUE)
hist (dist, breaks=seq(0,10+max(dist[!is.na(dist)]),10), freq=TRUE, xlab="Distance (in meters) to nearest safe well", ylab="", main="", mgp=c(2,.5,0))
#dev.off ()

# plots of model fit

jitter.binary <- function(a, jitt=.05){
  a + (1-2*a)*runif(length(a),0,jitt)
}

#postscript ("c:/books/multilevel/arsenic.logitfit.1new.a.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(dist, na.rm=TRUE)*1.02), c(0,1), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x), lwd=1, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
#dev.off ()

# histogram of As levels

#postscript ("c:/books/multilevel/arsenic.levels.a.ps", height=3, width=4, horizontal=TRUE)
hist (arsenic, breaks=seq(0,.25+max(arsenic[!is.na(arsenic)]),.25), freq=TRUE, xlab="Arsenic concentration in well water", ylab="", main="", mgp=c(2,.5,0))
#dev.off ()

# model with 2 predictors

fit.3 <- glm (switch ~ dist100 + arsenic, family=binomial(link="logit"))
display (fit.3)

#postscript ("c:/books/multilevel/arsenic.2variables.a.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(dist,na.rm=TRUE)*1.02), c(0,1), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (dist, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*.50), lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*1.00), lwd=.5, add=TRUE)
text (50, .27, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)
#dev.off ()

#postscript ("c:/books/multilevel/arsenic.2variables.b.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(arsenic,na.rm=TRUE)*1.02), c(0,1), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0.5+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
text (50, .78, "if dist = 0", adj=0, cex=.8)
text (200, .6, "if dist = 50", adj=0, cex=.8)
#dev.off ()

# including an interaction

fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic,
  family=binomial(link="logit"))

# centering the input variables

c.dist100 <- dist100 - mean (dist100)
c.arsenic <- arsenic - mean (arsenic)

fit.5 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,
  family=binomial(link="logit"))

#postscript ("c:/books/multilevel/arsenic.interact.a.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(dist,na.rm=TRUE)*1.02), c(0,1), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (dist, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*.50+coef(fit.4)[4]*(x/100)*.50), lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*1.00+coef(fit.4)[4]*(x/100)*1.00), lwd=.5, add=TRUE)
text (50, .29, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)
#dev.off ()

#postscript ("c:/books/multilevel/arsenic.interact.b.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(arsenic,na.rm=TRUE)*1.02), c(0,1), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0+coef(fit.4)[3]*x+coef(fit.4)[4]*0*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0.5+coef(fit.4)[3]*x+coef(fit.4)[4]*0.5*x), from=0.5, lwd=.5, add=TRUE)
text (.50, .78, "if dist = 0", adj=0, cex=.8)
text (2.00, .6, "if dist = 50", adj=0, cex=.8)
#dev.off ()

# adding social predictors

educ4 <- educ/4

fit.6 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  assoc + educ4, family=binomial(link="logit"))
display (fit.6)

fit.7 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  educ4, family=binomial(link="logit"))
display (fit.7)

c.educ4 <- educ4 - mean(educ4)

fit.8 <- glm (switch ~ c.dist100 + c.arsenic + c.educ4 + c.dist100:c.arsenic +
  c.dist100:c.educ4 + c.arsenic:c.educ4, family=binomial(link="logit"))
display (fit.8)

# plots of residuals

pred.8 <- fit.8$fitted.values

#postscript ("c:/books/multilevel/arsenic.logitresidsa.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,1), c(-1,1), xlab="Estimated  Pr (switching)", ylab="Observed - estimated", type="n", main="Residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
points (pred.8, switch-pred.8, pch=20, cex=.2)
#dev.off ()

binned.resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}

#postscript ("c:/books/multilevel/arsenic.logitresidsb.ps", height=3.5, width=4, horizontal=TRUE)
br.8 <- binned.resids (pred.8, switch-pred.8, nclass=40)$binned
plot(range(br.8[,1]), range(br.8[,2],br.8[,6],-br.8[,6]), xlab="Estimated  Pr (switching)", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.8[,1], br.8[,6], col="gray", lwd=.5)
lines (br.8[,1], -br.8[,6], col="gray", lwd=.5)
points (br.8[,1], br.8[,2], pch=20, cex=.5)
#dev.off ()

# compute error rates

error.rate <- mean(round(abs(switch-pred.8)))
error.rate.null <- mean(round(abs(switch-mean(pred.8))))

# more residual plots

#postscript ("c:/books/multilevel/arsenic.logitresids.2a.ps", height=3.5, width=4, horizontal=TRUE)
br <- binned.resids (dist, switch-pred.8, nclass=40)$binned
plot(range(br[,1]), range(br[,2],br[,6],-br[,6]), xlab="Distance to nearest safe well", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
n.within.bin <- length(y)/nrow(br)
lines (br[,1], br[,6], col="gray", lwd=.5)
lines (br[,1], -br[,6], col="gray", lwd=.5)
points (br[,1], br[,2], pch=20, cex=.5)
#dev.off ()

#postscript ("c:/books/multilevel/arsenic.logitresids.2b.ps", height=3.5, width=4, horizontal=TRUE)
br <- binned.resids (arsenic, switch-pred.8, nclass=40)$binned
plot(range(0,br[,1]), range(br[,2],br[,6],-br[,6]), xlab="Arsenic level", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br[,1], br[,6], col="gray", lwd=.5)
lines (br[,1], -br[,6], col="gray", lwd=.5)
points (br[,1], br[,2], pch=20, cex=.5)
#dev.off ()

# new model on log scale

log.arsenic <- log (arsenic)
c.log.arsenic <- log.arsenic - mean (log.arsenic)

fit.9 <- glm (switch ~ c.dist100 + c.log.arsenic + c.educ4 +
  c.dist100:c.log.arsenic + c.dist100:c.educ4 + c.log.arsenic:c.educ4,
  family=binomial(link="logit"))
display (fit.9)

fit.9a <- glm (switch ~ dist100 + log.arsenic + educ4 +
  dist100:log.arsenic + dist100:educ4 + log.arsenic:educ4,
  family=binomial(link="logit"))

# graphs for log model

#postscript ("c:/multilevel/arsenic.logmodel.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(arsenic,na.rm=TRUE)*1.02), c(0,1), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.9a)[1]+coef(fit.9a)[2]*0+coef(fit.9a)[3]*log(x)+coef(fit.9a)[4]*mean(educ4)+coef(fit.9a)[5]*0*log(x)+coef(fit.9a)[6]*0*mean(educ4)+coef(fit.9a)[7]*log(x)*mean(educ4)), from=.50, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.9a)[1]+coef(fit.9a)[2]*.5+coef(fit.9a)[3]*log(x)+coef(fit.9a)[4]*mean(educ4)+coef(fit.9a)[5]*.5*log(x)+coef(fit.9a)[6]*.5*mean(educ4)+coef(fit.9a)[7]*log(x)*mean(educ4)), from=.50, lwd=.5, add=TRUE)
text (.25, .80, "if dist = 0", adj=0, cex=.8)
text (2.00, .63, "if dist = 50", adj=0, cex=.8)
#dev.off ()

pred.9 <- fit.9$fitted.values

#postscript ("c:/books/multilevel/arsenic.logitresids.3b.ps", height=3.5, width=4, horizontal=TRUE)
br <- binned.resids (arsenic, switch-pred.9, nclass=40)$binned
plot(range(0,br[,1]), range(br[,2],br[,6],-br[,6]), xlab="Arsenic level", ylab="Average residual", type="n", main="Binned residual plot\nfor model with log (arsenic)", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
n.within.bin <- length(y)/nrow(br)
lines (br[,1], br[,6], col="gray", lwd=.5)
lines (br[,1], -br[,6], col="gray", lwd=.5)
points (br[,1], br[,2], pch=20, cex=.5)
#dev.off ()

# calculations for average predictive differences

# simple model
fit.10 <- glm (switch ~ dist100 + arsenic + educ4,
  family=binomial(link="logit"))
display (fit.10)

# avg pred diffs for distance to nearest safe well
b <- coef (fit.10)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic + b[4]*educ4) -
         invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4)
print (mean(delta))

# avg pred diffs for arsenic level
hi <- 1.0
lo <- 0.5
delta <- invlogit (b[1] + b[2]*dist100 + b[3]*hi + b[4]*educ4) -
         invlogit (b[1] + b[2]*dist100 + b[3]*lo + b[4]*educ4)
print (mean(delta))

# avg pred diffs for education
hi <- 3
lo <- 0
delta <- invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*hi) -
         invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*lo)
print (mean(delta))

# example model with interaction
fit.11 <- glm (switch ~ dist100 + arsenic + educ4 + dist100:arsenic,
  family=binomial(link="logit"))
display (fit.11)

b <- coef (fit.11)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic + b[4]*educ4 +
                   b[5]*hi*arsenic) -
         invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4 +
                   b[5]*lo*arsenic)
print (mean(delta))
