## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/arsenic

# The R codes & data files should be saved in the same directory for
# the source command to work

source("5.5_Logistic regression with interactions.R") # where variables were defined

## Fitting the model

fit.8 <- glm (switch ~ c.dist100 + c.arsenic + c.educ4 + c.dist100:c.arsenic +
  c.dist100:c.educ4 + c.arsenic:c.educ4, family=binomial(link="logit"))
display (fit.8)

## Residual Plot (Figure 5.13 (a))

pred.8 <- fit.8$fitted.values

plot(c(0,1), c(-1,1), xlab="Estimated  Pr (switching)", ylab="Observed - estimated", type="n", main="Residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
points (pred.8, switch-pred.8, pch=20, cex=.2)

### Binned residual Plot 

 ## Defining binned residuals

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

 ## Binned residuals vs. estimated probability of switching (Figure 5.13 (b))

br.8 <- binned.resids (pred.8, switch-pred.8, nclass=40)$binned
plot(range(br.8[,1]), range(br.8[,2],br.8[,6],-br.8[,6]), xlab="Estimated  Pr (switching)", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.8[,1], br.8[,6], col="gray", lwd=.5)
lines (br.8[,1], -br.8[,6], col="gray", lwd=.5)
points (br.8[,1], br.8[,2], pch=19, cex=.5)

 ## Plot of binned residuals vs. inputs of interest

  # distance (Figure 5.13 (a))

br.dist <- binned.resids (dist, switch-pred.8, nclass=40)$binned
plot(range(br.dist[,1]), range(br.dist[,2],br.dist[,6],-br.dist[,6]), xlab="Distance to nearest safe well", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.dist[,1], br.dist[,6], col="gray", lwd=.5)
lines (br.dist[,1], -br.dist[,6], col="gray", lwd=.5)
points (br.dist[,1], br.dist[,2], pch=19, cex=.5)

  # arsenic (Figure 5.13 (b))

br.arsenic <- binned.resids (arsenic, switch-pred.8, nclass=40)$binned
plot(range(0,br.arsenic[,1]), range(br.arsenic[,2],br.arsenic[,6],-br.arsenic[,6]), xlab="Arsenic level", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.arsenic[,1], br.arsenic[,6], col="gray", lwd=.5)
lines (br.arsenic[,1], -br.arsenic[,6], col="gray", lwd=.5)
points (br.arsenic[,1], br.arsenic[,2], pch=19, cex=.5)

## Log transformation

log.arsenic <- log (arsenic)
c.log.arsenic <- log.arsenic - mean (log.arsenic)

fit.9 <- glm (switch ~ c.dist100 + c.log.arsenic + c.educ4 +
  c.dist100:c.log.arsenic + c.dist100:c.educ4 + c.log.arsenic:c.educ4,
  family=binomial(link="logit"))
display (fit.9)

fit.9a <- glm (switch ~ dist100 + log.arsenic + educ4 +
  dist100:log.arsenic + dist100:educ4 + log.arsenic:educ4,
  family=binomial(link="logit"))

## Graph for log model fit.9a (Figure 5.15 (a))

plot(arsenic, switch.jitter, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.9a)[1]+coef(fit.9a)[2]*0+coef(fit.9a)[3]*log(x)+coef(fit.9a)[4]*mean(educ4)+coef(fit.9a)[5]*0*log(x)+coef(fit.9a)[6]*0*mean(educ4)+coef(fit.9a)[7]*log(x)*mean(educ4)), from=.50, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.9a)[1]+coef(fit.9a)[2]*.5+coef(fit.9a)[3]*log(x)+coef(fit.9a)[4]*mean(educ4)+coef(fit.9a)[5]*.5*log(x)+coef(fit.9a)[6]*.5*mean(educ4)+coef(fit.9a)[7]*log(x)*mean(educ4)), from=.50, lwd=.5, add=TRUE)
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
text (1.2, .8, "if dist = 0", adj=0, cex=.8)
text (1.8, .6, "if dist = 50", adj=0, cex=.8)

## Graph of binned residuals for log model fit.9 (Figure 5.15 (b))

pred.9 <- fit.9$fitted.values

br.fit.9 <- binned.resids (arsenic, switch-pred.9, nclass=40)$binned
plot(range(0,br.fit.9[,1]), range(br.fit.9[,2],br.fit.9[,6],-br.fit.9[,6]), xlab="Arsenic level", ylab="Average residual", type="n", main="Binned residual plot\nfor model with log (arsenic)", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.fit.9[,1], br.fit.9[,6], col="gray", lwd=.5)
lines (br.fit.9[,1], -br.fit.9[,6], col="gray", lwd=.5)
points (br.fit.9[,1], br.fit.9[,2], pch=19, cex=.5)

## Error rates

 # in general

error.rate <- mean((predicted>0.5 & y==0) | (predicted<0.5 & y==1))

 # for modell fit.9

error.rate <- mean((pred.9>0.5 & switch==0) | (pred.9<0.5 & switch==1))
