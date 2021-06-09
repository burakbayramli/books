######################## © CSIRO Australia 2005 ###############################
# Session 07:  Non-Linear Regression                                          #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

####################
# Stormer Viscometer Data

# Fitting the model
b <- coef(lm(Wt*Time ~ Viscosity + Time - 1,stormer))
names(b) <- c("beta", "theta")
b
storm.1 <- nls(Time ~ beta*Viscosity/(Wt - theta),stormer, start=b, trace=T)
summary(storm.1)

# Self starting models
library(MASS)   # only attach if haven't already attached this function
storm.init <- function(mCall, data, LHS) {
      v <- eval(mCall[["V"]], data)
      w <- eval(mCall[["W"]], data)
      t <- eval(LHS, data)
      b <- lsfit(cbind(v, t), t * w, int = F)$coef
      names(b) <- mCall[c("b", "t")]
     # browser()
      b
    }
NLSstormer <- selfStart( ~ b*V/(W-t), 		storm.init, c("b","t"))
args(NLSstormer)
tst <- nls(Time ~ NLSstormer(Viscosity, Wt, beta, theta), stormer, trace = T)
# Bootstrapping
tst$call$trace <- NULL
B <- matrix(NA, 500, 2)
r <- scale(resid(tst), scale = F)      # mean correct
f <- fitted(tst)
for(i in 1:500) {
    v <- f + sample(r, rep = T)
    B[i, ] <- try(coef(update(tst, v ~ .))) # guard!
}
cbind(Coef = colMeans(B), SD = sd(B))

# Parametric model counterparts
cbind(Coef = coef(tst), SD = sqrt(diag(vcov(tst))))
# Bayesian Bootstrap
b <- c(b = 29.1, th=2.21)
n <- nrow(stormer)
B <- matrix(NA, 1000, 2)

for(i in 1:1000) {
    # Exponential Weights
    w <- sqrt(rexp(n))
    # Uniform Weights
    #w <- runif(n,0,1)

    B[i, ] <- try(coef(nls(~w*(Time - b*Viscosity/(Wt - th)),data = stormer, start = b)))

  }
cbind(Coef = colMeans(B), SD = sd(B))


###################
# Muscle Dataset  (part of the MASS library)

# Fixed parameters
X <- model.matrix(~ Strip-1,muscle)
musc.1 <- nls(Length ~ cbind(X, X*exp(-Conc/th)),
		muscle, start = list(th = 1), algorithm = "plinear",
	 	trace = T)
b <- coef(musc.1)
b

# Conventional Fitting Algorithm
b <- as.vector(b) # remove names attribute
th <- b[1]
a <- b[2:22]
b <- b[23:43]
musc.2 <- nls(Length ~ a[Strip] + b[Strip]*exp(-Conc/th),
      	muscle, start = list(a = a, b = b, th = th),trace = T)
# Plotting the Result
range(muscle$Conc)
newdat <- expand.grid(Strip = levels(muscle$Strip),Conc = seq(0.25, 4, 0.05))
dim(newdat)
names(newdat)
newdat$Length <- predict(musc.2, newdat)
trellis.par.set(col.whitebg())
xyplot(Length ~ Conc | Strip, muscle, subscripts = T,
	 panel = function(x, y, subscripts, ...) {
    	panel.xyplot(x, y, ...)
    	ws <- as(muscle$Strip[subscripts[1]], "character")
    	wf <- which(newdat$Strip == ws)
    	xx <- newdat$Conc[wf]
    	yy <- newdat$Length[wf]
    	llines(xx, yy, col = "red")
    },
    ylim = range(newdat$Length, muscle$Length),as.table = T, col = "navy")

# Random Effects Version
musc.re <- nlme(Length ~ a + b*exp(-Conc/th),
		fixed = a+b+th~1, random = a+b~1|Strip,
		data = muscle, start=c(a = mean(a), b = mean(b), th = th))

# Composite Plot
newdat$L2 <- predict(musc.re, newdat)
xyplot(Length ~ Conc | Strip, muscle, subscripts = T,
   par.strip.text = list(lines=1,cex=0.7),
   panel =function(x, y, subscripts, ...) {
     panel.xyplot(x, y, ...)
     ws <- as(muscle$Strip[subscripts[1]], "character")
     wf <- which(newdat$Strip == ws)
     xx <- newdat$Conc[wf]
     yy <- newdat$Length[wf]
     llines(xx, yy, col = "gold")
     yy <- newdat$L2[wf]
     llines(xx, yy, lty=4, col = "navy")
    },
  ylim = range(newdat$Length, muscle$Length),as.table = T, col = "hotpink")





