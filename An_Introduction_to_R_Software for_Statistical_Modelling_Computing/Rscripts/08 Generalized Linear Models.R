######################## © CSIRO Australia 2005 ###############################
# Session 08:  Generalized Linear Models and Extensions                       #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

####################
# Example:  Budworm data

# Creating the data
options(contrasts = c("contr.treatment", "contr.poly")) # R default
ldose <- rep(0:5, 2)
numdead <- scan()
 1 4 9 13 18 20
 0 2 6 10 12 16

sex <- factor(rep(c("M", "F"), each = 6))
SF <- cbind(numdead, numalive = 20 - numdead)
Budworms <- data.frame(ldose, sex)
Budworms$SF <- SF
rm(sex, ldose, SF)

# Fitting an Initial Model
budworm.lg <- glm(SF ~ sex/ldose, family = binomial,data = Budworms, trace = T)
summary(budworm.lg, cor = F)
# Plotting
with(Budworms,{
  plot(c(1,32), c(0,1), type = "n", xlab = "dose", log = "x",
    axes = F,ylab = "Pr(Death)")
  axis(1, at = 2^(0:5))
  axis(2)
  points(2^ldose[1:6], numdead[1:6]/20, pch = 4)
  points(2^ldose[7:12], numdead[7:12]/20, pch = 1)
  ld <- seq(0, 5, length = 100)
  lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
     sex = factor(rep("M", length(ld)), levels = levels(sex))),
     type = "response"), col = "orange", lwd = 2)
  lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
     sex = factor(rep("F", length(ld)), levels = levels(sex))),
     type = "response"), lty = 2, col = "blue", lwd = 2)
  })


# Is Sex Significant?
budworm.lgA <- update(budworm.lg, . ~ sex/I(ldose - 3))
summary(budworm.lgA, cor = F)$coefficients

# Checking for curvature
anova(update(budworm.lgA, . ~ . + sex/I((ldose - 3)^2)),test = "Chisq")

# Test for Parallelism
budworm.lg0 <- glm(SF~sex+ldose-1,family=binomial,Budworms,trace=T)
anova(budworm.lg0,budworm.lgA,test="Chisq")


######################
# Effective Dosagese (V&R p193)

# dose.p is in MASS library
dose.p <-
function (obj, cf = 1:2, p = 0.5)
{
    eta <- family(obj)$linkfun(p)
    b <- coef(obj)[cf]
    x.p <- (eta - b[1])/b[2]
    names(x.p) <- paste("p = ", format(p), ":", sep = "")
    pd <- -cbind(1, x.p)/b[2]
    SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
    res <- structure(x.p, SE = SE, p = p)
    class(res) <- "glm.dose"
    browser()
    res
}


print.glm.dose <- function(x, ...){
    M <- cbind(x, attr(x, "SE"))
    dimnames(M) <- list(names(x), c("Dose", "SE"))
    x <- M
    NextMethod("print")
}
dose.p(budworm.lg0, cf = c(1, 3), p = 1:3/4)


#########################
# Low birthweight

# Manipulating the Data
options(contrasts = c("contr.treatment", "contr.poly"))
attach(birthwt)
race <- factor(race, labels = c("white", "black", "other"))
table(ptl)
ptd <- factor(ptl > 0)
table(ftv)
ftv <- factor(ftv)
levels(ftv)[ - (1:2)] <- "2+"
table(ftv)
bwt <- data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
     ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach("birthwt")
rm(race, ptd, ftv)

# Initial Model
birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
dropterm(birthwt.glm, test = "Chisq")

# Interactions
birthwt.step2 <- stepAIC(birthwt.glm,  ~ .^2 + I(scale(age)^2) +
  I(scale(lwt)^2), trace = F)
birthwt.step2$anova
dropterm(birthwt.step2, test = "Chisq")

# Checking for linearity on age within ftv
attach(bwt)
BWT <- expand.grid(age=14:45, lwt = mean(lwt),
	race = factor("white", levels = levels(race)),
	smoke = c(T,F),ptd = factor(c(T,F)),ht = c(T,F),ui = c(T,F),ftv = levels(ftv))
detach("bwt")

nsAge <- function(x)
	splines::ns(x, knots = quantile(bwt$age, 1:2/3),Boundary.knots = range(bwt$age))

# Hard wiring the knot placements
birthwt.glm2 <- glm(low ~ lwt + ptd + ht + smoke * ui + ftv/nsAge(age),
             binomial, bwt, trace = TRUE)

# Sloppy Version
birthwt.glm2 <- glm(low ~ lwt + ptd + ht + smoke * ui + ftv/splines::ns(age, df = 3),
             binomial, bwt, trace = TRUE)


prob <- predict(birthwt.glm2, BWT, type = "resp")
xyplot(prob ~ age | ftv, BWT, type = "l",
	subset = smoke == F & ptd == F & ht == F & ui == F, as.table = T,
  ylim = c(0, 1), ylab = "Pr(Low bwt)")
  
###########################
# GLM Extensions
###########################

##################
# Quine Dataset
quine.po1 <- glm(Days ~ .^4, poisson, quine, trace = T)
summary(quine.po1, cor = F)

# Initial Value for theta
t0 <- 1/var(quine$Days/fitted(quine.po1))
t0

# Initial Negative Binomial Fit and Test
quine.nb1 <- glm.nb(Days ~ Eth * Lrn * Age * Sex, data = quine,
  init.theta = t0, trace = 2)
quine.nb1$call$trace <- F  # turn off tracing
dropterm(quine.nb1, test = "Chisq")

# Backwards Elimination to a Final Model
quine.nb2 <- update(quine.nb1, . ~ . - Eth:Lrn:Age:Sex)
dropterm(quine.nb2, test = "Chisq", k = log(nrow(quine)))
quine.nb3 <- update(quine.nb2, . ~ . - Eth:Age:Sex)
dropterm(quine.nb3, test = "Chisq", k = log(nrow(quine)))
quine.nb4 <- update(quine.nb3, . ~ . - Lrn:Age:Sex)
dropterm(quine.nb4, test = "Chisq", k = log(nrow(quine)))
quine.nb5 <- update(quine.nb4, . ~ . - Lrn:Age:Eth)
dropterm(quine.nb5, test = "Chisq", k = log(nrow(quine)))
quine.nb6 <- update(quine.nb5, . ~ . - Lrn:Age)
dropterm(quine.nb6, test = "Chisq", k = log(nrow(quine)))
quine.nb7 <- update(quine.nb6, . ~ . - Eth:Age)
dropterm(quine.nb7, test = "Chisq", k = log(nrow(quine)))
quine.check <- glm.nb(Days ~ Sex/(Age + Eth * Lrn), quine)
deviance(quine.nb7)
deviance(quine.check)
range(fitted(quine.nb7) - fitted(quine.check))

# Diagnostic Checks
fv <- fitted(quine.nb7)
rs <- resid(quine.nb7, type = "deviance")
pv <- predict(quine.nb7)
par(mfrow = c(2,2))
plot(fv, rs, xlab = "fitted values",ylab = "deviance residuals")
abline(h = 0, lty = 4, lwd = 2, col = "orange")
qqnorm(rs, ylab = "sorted deviance residuals")
qqline(rs, col = "orange", lwd = 2, lty = 4)
par(mfrow=c(1,1))

# Fixing Theta at a Constant Value
quine.glm1 <- glm(Days ~ Eth * Sex * Lrn * Age, negative.binomial(theta = t0),
  data = quine, trace = F)
quine.step <- stepAIC(quine.glm1, k = log(nrow(quine)),	trace = F)
dropterm(quine.step, test = "Chisq")

#########################
# Multinomial Models

# Cophenhagen Housing Data
hous.glm0 <- glm(Freq ~ Infl*Type*Cont, poisson, housing)
hous.glm1 <- update(hous.glm0, .~.+Sat)
anova(hous.glm0, hous.glm1, test = "Chisq")
addterm(hous.glm1, . ~ . + Sat * (Infl + Type + Cont), test = "Chisq")

hous.glm2 <- update(hous.glm1, .~.+Sat*(Infl+Type+Cont))
# Find a table of estimated probabilities
attach(housing)
levs <- lapply(housing[, -5], levels)
dlev <- sapply(levs, length)

ind <- do.call("cbind", lapply(housing[, -5], function(x) match(x, levels(x))))
detach("housing")

RF <- Pr <- array(0, dim = dlev, dimnames = levs)
RF[ind] <- housing$Freq
tots <- rep(apply(RF, 2:4, sum), each = 3)

RF <- RF/as.vector(tots)
RF

Pr[ind] <- fitted(hous.glm2)
Pr <- Pr/as.vector(tots)
round(Pr,2)

# Multinomial Model
require(nnet)
hous.mult <- multinom(Sat ~ Infl + Type + Cont, data = housing,
  weights = Freq, trace = T)
round(fitted(hous.mult), 2)
h1 <- t(fitted(hous.mult)[seq(3, 72, 3),  ])
range(h1 - as.vector(Pr))

# Proportional Odds Model
hous.polr <- polr(Sat ~ Infl+Type+Cont, data = housing, weights = Freq)
plot(fitted(hous.polr), fitted(hous.mult))
abline(0, 1, col="orange", lty=4, lwd=1)
hous.polr2 <- stepAIC(hous.polr, ~.^2, k = log(24))
hous.polr2$call$formula






















