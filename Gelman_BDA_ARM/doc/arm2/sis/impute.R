# Regression-based imputation for the Social Indicators Survey

# General function for creating a completed data vector using imputations

impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

# 1.  Deterministic imputation

# Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)

zero.earnings <- workhrs==0 & workmos==0
earnings.top[zero.earnings] <- 0

# Create a little dataset with all our redefined variables!

SIS <- data.frame (cbind (earnings, earnings.top, male, over65,
  white, immig, educ_r, workmos, workhrs.top,
  any.ssi, any.welfare, any.charity))

# Impute subset of earnings that are nonzero:  linear scale

lm.imp.1 <- lm (earnings ~ male + over65 + white + immig +
  educ_r + workmos + workhrs.top + any.ssi + any.welfare + any.charity,
  data=SIS, subset=earnings>0)
display (lm.imp.1)
pred.1 <- predict (lm.imp.1, SIS)
earnings.imp.1 <- impute (earnings, pred.1)

# Impute subset of earnings that are nonzero:  square root scale and topcoding

lm.imp.2.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white + immig +
  educ_r + workmos + workhrs.top + any.ssi + any.welfare + any.charity,
  data=SIS, subset=earnings>0)
display (lm.imp.2.sqrt)
pred.2.sqrt <- predict (lm.imp.2.sqrt, SIS)
pred.2 <- topcode (pred.2.sqrt^2, 100)
earnings.imp.2 <- impute (earnings.top, pred.2)

###############################################################################

# 2.  Random imputation

# Linear scale (use fitted model lm.imp.1)

pred.3 <- rnorm (n, predict (lm.imp.1, SIS), sigma.hat (lm.imp.1))
earnings.imp.3 <- impute (earnings, pred.3)

# Square root scale and topcoding (use fitted model lm.imp.2)

pred.4.sqrt <- rnorm (n, predict (lm.imp.2.sqrt, SIS), sigma.hat (lm.imp.2.sqrt))
pred.4 <- topcode (pred.4.sqrt^2, 100)
earnings.imp.4 <- impute (earnings.top, pred.4)

###############################################################################

# 3.  Histograms and scatterplots of data and imputations

postscript ("c:/books/multilevel/impute.hist2.ps", horizontal=T)
par (mar=c(7,6,4,3)+.1)
hist (earnings.top[earnings>0], mgp=c(5,2,0),
      breaks=seq(0,100,10), xlab="earnings", ylab="", cex.lab=3,
      cex.axis=3, cex.main=3, main="Observed earnings (excluding 0's)")
dev.off ()

postscript ("c:/books/multilevel/impute.hist3.ps", horizontal=T)
par (mar=c(7,6,4,3)+.1)
hist (earnings.imp.2[is.na(earnings)], breaks=seq(0,100,10),mgp=c(5,2,0),
      xlab="earnings", ylab="", cex.lab=3, ylim=c(0,48),
      cex.axis=3, cex.main=3, main="Deterministic imputation of earnings")
dev.off ()

postscript ("c:/books/multilevel/impute.hist4.ps", horizontal=T)
par (mar=c(7,6,4,3)+.1)
hist (earnings.imp.4[is.na(earnings)], breaks=seq(0,100,10),mgp=c(5,2,0),
      xlab="earnings", ylab="", cex.lab=3, ylim=c(0,48),
      cex.axis=3, cex.main=3, main="Random imputation of earnings")
dev.off ()

postscript ("c:/books/multilevel/impute.scat1.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (range (earnings.imp.2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=2.5, cex.axis=2.5, cex.main=2.5,
      main="Deterministic imputation", type="n")
points (earnings.imp.2[is.na(earnings)], earnings.imp.2[is.na(earnings)], cex=1.5, pch=19)
points (pred.2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")
dev.off ()

postscript ("c:/books/multilevel/impute.scat2.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
plot (range (earnings.imp.2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=2.5, cex.axis=2.5, cex.main=2.5,
      main="Random imputation", type="n")
points (earnings.imp.2[is.na(earnings)], earnings.imp.4[is.na(earnings)], cex=1.5, pch=19)
points (pred.2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")
dev.off ()

###############################################################################

# 4.  Two-stage imputation model

# Fit the 2 models

glm.sign <- glm (I(earnings>0) ~ male + over65 + white + immig +
  educ_r + any.ssi + any.welfare + any.charity,
  data=SIS, family=binomial(link=logit))
display (glm.sign)
lm.ifpos.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white + immig +
  educ_r + any.ssi + any.welfare + any.charity,
  data=SIS, subset=earnings>0)  # (same as lm.imp.2 from above)
display (lm.ifpos.sqrt)

# Predict the sign and then the earnings (if positive)

pred.sign <- rbinom (n, 1, predict (glm.sign, SIS, type="response"))
pred.pos.sqrt <- rnorm (n, predict (lm.ifpos.sqrt, SIS),
  sigma.hat(lm.ifpos.sqrt))
pred.pos <- topcode (pred.pos.sqrt^2, 100)
earnings.imp <- impute (earnings, pred.sign*pred.pos)

###############################################################################

# 5.  Iterative regression imputation

# starting values

interest.imp <- random.imp (interest)
earnings.imp <- random.imp (earnings)

# simplest regression imputation

n.sims <- 10
for (s in 1:n.sims){
  lm.1 <- lm (earnings ~ interest.imp + male + over65 + white +
    immig + educ_r + workmos + workhrs.top + any.ssi + any.welfare +
    any.charity)
  pred.1 <- rnorm (n, predict(lm.1), sigma.hat(lm.1))
  earnings.imp <- impute (earnings, pred.1)

  lm.1 <- lm (interest ~ earnings.imp + male + over65 + white +
    immig + educ_r + workmos + workhrs.top + any.ssi + any.welfare +
    any.charity)
  pred.1 <- rnorm (n, predict(lm.1), sigma.hat(lm.1))
  interest.imp <- impute (interest, pred.1)
}

