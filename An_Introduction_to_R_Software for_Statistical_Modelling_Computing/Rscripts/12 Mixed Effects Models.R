######################## © CSIRO Australia 2005 ###############################
# Session 12:  Mixed Effects Models                                           #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

####################################
# Linear Mixed Effects Models
####################################


#################################
# Petroleum Data

# The petrol data displayed
xyplot(Y ~ EP | No, petrol,panel = function(x, y, ...) {
		panel.xyplot(x, y, ...)
		panel.lmline(x, y, col = 3, lty = 4)
	},
	as.table = T, aspect = "xy", xlab = "End point", ylab = "Yield (%)")

# Parallel Slopes
petrol.lm1 <- aov(Y ~ No/EP, petrol)
petrol.lm2 <- aov(Y ~ No + EP, petrol)
anova(petrol.lm2, petrol.lm1)

# Difference in Intercepts
petrol.lm3 <- aov(Y ~ . - No, petrol)
anova(petrol.lm3, petrol.lm2)

# Looking at all three models
tmp <- update(petrol.lm2, .~.-1)
a0 <- predict(tmp, type="terms")[, "No"]
b0 <- coef(tmp)["EP"]

a <- with(petrol,cbind(1, SG, VP, V10)) %*% coef(petrol.lm3)[1:4]
b <- coef(petrol.lm3)["EP"]

xyplot(Y ~ EP | No, petrol, subscripts = T,
	panel = function(x, y,  subscripts, ...) {
		panel.xyplot(x, y, ...)
		panel.lmline(x, y, col = "green", lty = 1)
		panel.abline(a0[subscripts][1], b0, col = "blue", lty = 2)
		panel.abline(a[subscripts][1], b, col = "red", lty = 3)
	}, as.table = T, aspect = "xy", xlab = "End point",
	ylab = "Yield (%)",
	key=list(lines= list(lty = 1:3, col = c("green","blue","red")),columns=3,
                  text=list(c("separate","parallel","explained"))))


# Adding a Random Term
require(nlme)
petrol.lme <- lme(Y ~ SG + VP + V10 + EP, data = petrol, random = ~1 | No)
summary(petrol.lme)

# Comparison with previous model
round(summary.lm(petrol.lm3)$coef, 5)

# Random Slopes
petrol.lme2 <- lme(Y ~ SG + VP + V10 + EP, data = petrol,random = ~ 1 + EP | No)
summary(petrol.lme2)

####################################
# Generalized Linear Mixed Effects Models
####################################

#######################
# Recovery of Benthos

# Reading in the data & some functions
Species <- scan("SpeciesNames.csv", what = "")
Benthos <- read.csv("Benthos.csv")
match(Species, names(Benthos))

bin <- function(fac, all = F) {
 #
 # binary matrix for a factor
 #
 fac <- as.factor(fac)
 X <- outer(fac, levels(fac), "==") + 0
 dimnames(X) <- list(NULL,paste("B",1:ncol(X),sep=""))
 if(all) X else X[, -1]
}
nsi <- function(x, k = 3, Intensity = Benthos$Intensity) {
#
# natural spline in intensity
#
  knots <- as.vector(quantile(unique(Intensity), 0:k/k))
  splines::ns(x, knots = knots[2:k], Boundary.knots = knots[c(1, k + 1)])
}

guard <- function(x) ifelse(x < -5, NA, x)

# Setting up the Prediction Data
vals <- with(Benthos, tapply(Intensity, Impact, mean))
msa <- with(Benthos, mean(SweptArea))

newData <- Benthos[, c("Cruise", "Plot", "Months", "Treatment",
                       "Time", "Impact", "Topography", "Unity")]
newData <- transform(newData,
           Months = as.numeric(as.character(Months)),
           Intensity = vals[factor(Impact)],
           SweptArea = rep(msa, nrow(Benthos)))
newData <- with(newData, newData[order(Months), ])

# Key Species
print(Species, quote = F)

# Fitting Multiple Similar Species
fitCommand <- Quote({
fm <- glmmPQL(Species ~ Topography +
	I(Impact * cbind(nsi(Intensity), bin(Time))) +
	offset(log(SweptArea)),
	random = ~1|Cruise/Plot,
	family = poisson, data = Benthos, verbose = T, maxit=100)
})

spno <- 1

### --- start of (manual) main loop

mname <- paste("GLMM", Species[spno], sep=".")
sname <- Species[spno]
thisFitCommand <- do.call("substitute", list(fitCommand,
	list(fm = as.name(mname), Species = as.name(sname))))

eval(thisFitCommand)
print(spno <- spno + 1)

### --- end of main loop
objects(pat = "GLMM.*")

# Plots
pfm <- predict(GLMM.Alcyonacea, newData) + log(newData$SweptArea)
pfm0 <- predict(GLMM.Alcyonacea, newData, level=0) + log(newData$SweptArea)
graphics.off()

trellis.device()
trellis.par.set(col.whitebg())
xyplot(guard(pfm) ~ Months|Treatment*Topography, newData, groups = Plot,
  panel = panel.superpose, type="b",
	main = "Alcyonacea", ylab = "log(mean)",
	sub = "fixed and random effects")

trellis.device()
trellis.par.set(col.whitebg())
xyplot(exp(pfm0) ~ Months|Treatment*Topography, newData, groups = Plot,
  panel = panel.superpose, type="b",
	main = "Alcyonacea", ylab = "mean",
	sub = "fixed effects only")


# Variance Components
summary(GLMM.Alcyonacea, corr = F)

########################
# Muscle Dataset

# Plotting
xyplot(log(Length) ~ Conc | Strip, muscle, as.table = T,
   xlab = "CaCl concentration")

# Initial Fit: Fixed Effects
X <- model.matrix(~Strip - 1, muscle)

muscle.nls1 <- nls(log(Length) ~ cbind(X, X*rho^Conc), muscle,
     start = c(rho = 0.1),
     algorithm = "plinear", trace = T)
b <- as.vector(coef(muscle.nls1))

init <- list(rho = b[1],alpha = b[2:22], beta = b[23:43])
muscle.nls2 <- nls(log(Length) ~ alpha[Strip] + beta[Strip]*rho^Conc, muscle,
            start = init, trace = T)

# Prediction and Presentation of the fit
Muscle <- expand.grid(Conc = seq(0.25, 4, len=20),
   Strip = levels(muscle$Strip), Length = 0)
Muscle <- rbind(muscle, Muscle)
Muscle <- with(Muscle, Muscle[order(Strip, Conc), ])
Muscle$fit <- predict(muscle.nls2, Muscle)

xyplot(fit ~ Conc|Strip, Muscle, type = "l", subscripts = T,
 prepanel = function(x, y, subscripts)
	 list(xlim = range(x),ylim = range(y, Muscle$fit[subscripts]),
   dx = NULL, dy=NULL),
 panel = function(x, y, subscripts, ...) {
	panel.xyplot(x, y, ...)
	panel.points(x, log(Muscle$Length[subscripts]))
 },
  as.table = T, ylab = "log(Length)",xlab = "CaCl concentration")
  
# Fitting a Mixed Effects Model
ival <- sapply(init, mean)

muscle.nlme <- nlme(log(Length) ~ alpha + beta*rho^Conc, muscle,
   fixed = rho+alpha+beta ~ 1, random = alpha + beta ~ 1|Strip, start = ival)

Muscle$RandomFit <- predict(muscle.nlme, Muscle)

# Fitting a Mixed Effects Model
xyplot(RandomFit ~ Conc|Strip, Muscle, type = "l",
	subscripts = T,
  xlim = with(Muscle,range(Conc)),
  ylim = with(Muscle,range(RandomFit,fit,log(muscle$Length))),
  par.strip.text = list(lines=1,cex=0.7),
  panel = function(x, y, subscripts, ...) {
	  panel.xyplot(x, y, ...)
	  panel.lines(x, Muscle$fit[subscripts], col=2,lty=2)
	  panel.points(x, log(Muscle$Length[subscripts]))
  }, as.table = T, ylab = "log(Length)",
		xlab = "CaCl concentration")

# Summary of Fit
summary(muscle.nlme)













   


