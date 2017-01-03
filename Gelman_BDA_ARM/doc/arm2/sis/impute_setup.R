# Read in data from wave 3 of the Social Indicators Survey

wave3 <- read.table ("siswave3v4impute3.csv", header=T, sep=",")
attach.all (wave3)
n <- nrow (wave3)

# Helpful little functions

random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing)
  return (imputed)
}

topcode <- function (a, top){
  return (ifelse (a>top, top, a))
}

# missing codes:  -9: refused/dk to say if you have this source
#                 -5: you said you had it but refused/dk the amount

# earnings variables:

# rearn:  respondent's earnings
# tearn:  spouse's earnings
# pearn:  earnings of primary wage-earner in family
# searn:  earnings of secondary wage-earner in family
# unemp:  unemployment, veteran's benefits, worker comp for entire family
# socsec:  social security of entire family
# pension:  pension, retirement income of entire family
# interest:  interest of entire family
# income:  rental income of entire family
# alimony:  alimony child support for entire family
# giftmon:  gift income for entire family
# ssi:  ssi for entire family
# welfare:  public assistance for entire family
# charity:  income received from charity for entire family

# demographics:

# earners:  #earners in family (0,1,2)
# sex:  male=1, female=2
# race of respondent:  1=white, 2=black, 3=hispanic(nonblack), 4=other
# famchild:  #kids (0-8)
# anych6:  any children under 6?
# immig:  0 if respondent is U.S. citizen, 1 if not
# rdisab:  is respondent disabled?
# cdisab:  is focal child disabled?
# educ_r:  respondent's education (1=no hs, 2=hs, 3=some coll, 4=college grad)
# r_age:  respondent's age (18-97)
# DON'T USE primary:  -9=missing, 0=spouse, 1=respondent is primary earner  
# marrcoh:  0=single, 1=married/cohabitating
# workmos:  primary earner's months worked last year
# workhrs:  primary earner's hours/week worked last year

white <- ifelse (race==1, 1, 0)
white[is.na(race)] <- 0
male <- ifelse (sex==1, 1, 0)
over65 <- ifelse (r_age>65, 1, 0)
immig[is.na(immig)] <- 0
educ_r[is.na(educ_r)] <- 2.5
earners[is.na(earners)] <- 1
no_earners <- ifelse (earners==0, 1, 0)
workhrs.top <- topcode (workhrs, 40)

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
earnings.orig <- earnings
earnings[workmos==0] <- 0
retirement <- na.fix(socsec) + na.fix(pension)
interest <- na.fix(interest)
assistance <- na.fix(unemp) + na.fix(ssi) + na.fix(welfare) + na.fix(charity)
other <- na.fix(alimony) + na.fix(giftmon)

# summary variables for various income supports

any.unemp <- is.any (unemp)
any.ssi <- is.any (ssi)
any.welfare <- is.any (welfare)
any.charity <- is.any (charity)

# transforming and topcoding the different sources of income

earnings <- earnings/1000
retirement <- retirement/1000
interest <- interest/1000
assistance <- assistance/1000
other <- other/1000

earnings.top <- topcode (earnings, 100)
retirement.top <- topcode (retirement, 100)
interest.top <- topcode (interest, 100)
assistance.top <- topcode (assistance, 10)
other.top <- topcode (other, 10)
