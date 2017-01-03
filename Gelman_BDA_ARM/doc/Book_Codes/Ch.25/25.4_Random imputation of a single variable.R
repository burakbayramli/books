## Read the the Social Indicators Survey data 
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sis

library("arm")
wave3 <- read.table ("siswave3v4impute3.csv", header=T, sep=",")
attach.all (wave3)
n <- nrow (wave3)

 # missing codes:  -9: refused/dk to say if you have this source
 #                 -5: you said you had it but refused/dk the amount

 # Variables description:

 # rearn:  respondent's earnings
 # tearn:  spouse's earnings 
 # ssi:  ssi for entire family
 # welfare:  public assistance for entire family
 # charity:  income received from charity for entire family
 # sex:  male=1, female=2
 # race of respondent:  1=white, 2=black, 3=hispanic(nonblack), 4=other
 # immig:  0 if respondent is U.S. citizen, 1 if not
 # educ_r:  respondent's education (1=no hs, 2=hs, 3=some coll, 4=college grad)
 # DON'T USE primary:  -9=missing, 0=spouse, 1=respondent is primary earner  
 # workmos:  primary earner's months worked last year
 # workhrs:  primary earner's hours/week worked last year

white <- ifelse (race==1, 1, 0)
white[is.na(race)] <- 0
male <- ifelse (sex==1, 1, 0)
over65 <- ifelse (r_age>65, 1, 0)
immig[is.na(immig)] <- 0
educ_r[is.na(educ_r)] <- 2.5

 # set up some simplified variables to work with
na.fix <- function (a) {
  ifelse (a<0 | a==999999, NA, a)
}

is.any <- function (a) {
  any.a <- ifelse (a>0, 1, 0)
  any.a[is.na(a)] <- 0
  return(any.a)
}

workmos <- workmos
earnings <- na.fix(rearn) + na.fix(tearn)
earnings[workmos==0] <- 0

 # summary variables for various income supports
any.ssi <- is.any (ssi)
any.welfare <- is.any (welfare)
any.charity <- is.any (charity)

 # transforming the different sources of income
earnings <- earnings/1000

## Simple random imputation
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

earnings.imp <- random.imp (earnings)

## Zero coding and topcoding
topcode <- function (a, top){
  return (ifelse (a>top, top, a))
}

earnings.top <- topcode (earnings, 100)
workhrs.top <- topcode (workhrs, 40)

 # plot figure 25.1a
hist (earnings.top[earnings>0], xlab="earnings", ylab="", 
  main="Observed earnings (excluding 0's)")

## Using regression predictions to perform deterministic imputation

 # create a little dataset with all redefined variables
sis <- data.frame (cbind (earnings, earnings.top, male, over65, white, 
  immig, educ_r, workmos, workhrs.top, any.ssi, any.welfare, any.charity))

 # fit a regression to positive values of earnings
lm.imp.1 <- lm (earnings ~ male + over65 + white + immig + educ_r + 
   workmos + workhrs.top + any.ssi + any.welfare + any.charity,
   data=sis, subset=earnings>0)
display (lm.imp.1)

 # get predictions
pred.1 <- predict (lm.imp.1, sis)

 # function to create a completed dataset by imputing the predictions
impute <- function (a, a.impute){
   ifelse (is.na(a), a.impute, a)
}
earnings.imp.1 <- impute (earnings, pred.1)  # use it to inpute missing
                                             # earnings

## Transforming and topcoding
lm.imp.2.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white + immig +
  educ_r + workmos + workhrs.top + any.ssi + any.welfare + any.charity,
  data=sis, subset=earnings>0)
display (lm.imp.2.sqrt)
pred.2.sqrt <- predict (lm.imp.2.sqrt, sis)
pred.2 <- topcode (pred.2.sqrt^2, 100)
earnings.imp.2 <- impute (earnings.top, pred.2)

 # plot figure 25.1b
hist (earnings.imp.2[is.na(earnings)], xlab="earnings", ylab="", 
  main="Deterministic imputation of earnings")

## Random regression imputation
pred.4.sqrt <- rnorm (n, predict (lm.imp.2.sqrt, sis), sigma.hat (lm.imp.2.sqrt))
pred.4 <- topcode (pred.4.sqrt^2, 100)
earnings.imp.4 <- impute (earnings.top, pred.4)

 # plot figure 25.1c
hist (earnings.imp.4[is.na(earnings)], xlab="earnings", ylab="", 
  main="Random imputation of earnings")

## Plots figure 25.2
par (mar=c(5,5,4,2)+.1)
plot (range (earnings.imp.2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=1.1, cex.axis=1.1, cex.main=1.2,
      main="Deterministic imputation", type="n")
points (earnings.imp.2[is.na(earnings)], earnings.imp.2[is.na(earnings)], cex=1.1, pch=19)
points (pred.2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")

par (mar=c(5,5,4,2)+.1)
plot (range (earnings.imp.2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=1.1, cex.axis=1.1, cex.main=1.2,
      main="Random imputation", type="n")
points (earnings.imp.2[is.na(earnings)], earnings.imp.4[is.na(earnings)], cex=1.1, pch=19)
points (pred.2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")

## Two-stage imputation model

 # fit the 2 models
glm.sign <- glm (I(earnings>0) ~ male + over65 + white + immig +
  educ_r + any.ssi + any.welfare + any.charity,
  data=sis, family=binomial(link=logit))
display (glm.sign)
lm.ifpos.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white + immig +
  educ_r + any.ssi + any.welfare + any.charity,
  data=sis, subset=earnings>0)  # (same as lm.imp.2 from above)
display (lm.ifpos.sqrt)

 # predict the sign and then the earnings (if positive)
pred.sign <- rbinom (n, 1, predict (glm.sign, sis, type="response"))
pred.pos.sqrt <- rnorm (n, predict (lm.ifpos.sqrt, sis),
  sigma.hat(lm.ifpos.sqrt))
pred.pos <- topcode (pred.pos.sqrt^2, 100)
earnings.imp <- impute (earnings, pred.sign*pred.pos)



