
# --- Roller data example ---
library(DAAG)
data(roller)
roller.lm <- lm(depression ~ weight, data=roller)
summary(roller.lm)$coefficients
#             Estimate Std. Error    t value    Pr(>|t|)
#(Intercept) -2.087148  4.7542813 -0.4390038 0.672274166
#weight       2.666746  0.7002426  3.8083171 0.005175013
coefs <- summary(roller.lm)$coefficients
# Individual 95% CI's for the true intercept and slope:
for (i in 1:2){
  print(coefs[i,1] + qt(c(.025, .975), 8)* coefs[i,2])
  }
#[1] -13.050540   8.876245  # intercept
#[1] 1.051984 4.281508      # slope
# Simultaneous Bonferroni 95% CI's for the true intercept and slope:
for (i in 1:2){
  print(coefs[i,1] + qt(c(.0125, .9875), 8)* coefs[i,2])
  }
# [1] -15.16866  10.99437  # intercept
# [1] 0.7400118 4.5934800  # slope
# Note that these intervals are wider than the individual intervals.
# The theory says that we can be at least 95% confident that both
#  of these intervals contain the true parameter values... assuming
#  the linear model is specified correctly -- a fairly safe for these
#  data.


# --- Seal data example --- 
source("Ch10.R")
seal1.lm <- lm(age~weight+heart+liver+stomach+kidney, data=cfseal1)
coefs <- summary(seal1.lm)$coefficients
for (i in 1:6){
  print(coefs[i,1] + qt(c(.05/(12), 1-.05/12), 8)* coefs[i,2])
  }
#[1] -22.19440  20.41744      intercept
#[1] -1.162762  2.248613      weight coef
#[1] -0.1502171  0.1029540    heart coef
#[1] -0.06700146  0.01200513    liver coef
#[1] -0.06003183  0.13976137    stomach coef
#[1] -0.1102167  0.4028246      kidney coef

# The above intervals are all true with 95% confidence, assuming
# the model is specified correctly -- not a very safe assumption
# here -- we need to check this more carefully.


