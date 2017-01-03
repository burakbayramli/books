############
#
# Note: Some of the examples in this handout were computed using
# an older version of R, so there may be some numbers that are different
# in the lower-order digits.  Please let me know if there are any
# differences big enough to change any conclusions...
#
# -BJ
#

############
#
# Box and Tiao (1973) analyse data first presented by Davies (1967)
# concerning batch to batch variation in yields of dyestuff. The data
# (shown below) arise from a balanced experiment whereby the total
# product yield was determined for 5 samples from each of 6 randomly
# chosen batches of raw material.
# 
# 	Batch		Yield (in grams)			
# 	_______________________________________
# 	1	1545	1440	1440	1520	1580
# 	2	1540	1555	1490	1560	1495	
# 	3	1595	1550	1605	1510	1560
# 	4	1445	1440	1595	1465	1545
# 	5	1595	1630	1515	1635	1625
# 	6	1520	1455	1450	1480	1445
# 
# The object of the study was to determine the relative importance of
# between batch variation versus variation due to sampling and analytic
# errors.  (the story and the data were taken from WinBUGS examples...)

dyes <- matrix(scan(sep=","),byrow=T,ncol=5)
1545, 1440, 1440, 1520, 1580
1540, 1555, 1490, 1560, 1495
1595, 1550, 1605, 1510, 1560
1445, 1440, 1595, 1465, 1545
1595, 1630, 1515, 1635, 1625
1520, 1455, 1450, 1480, 1445

dyes <- data.frame(cbind(as.factor(1:6),dyes))
dimnames(dyes)[[2]] <- c("Batch",paste("Y",1:5,sep=""))
dyes

# A more convenient form is the regression form of the data frame...

dyes <- scan(sep=",")
1545, 1440, 1440, 1520, 1580
1540, 1555, 1490, 1560, 1495
1595, 1550, 1605, 1510, 1560
1445, 1440, 1595, 1465, 1545
1595, 1630, 1515, 1635, 1625
1520, 1455, 1450, 1480, 1445

dyes <- data.frame(Batch=rep(1:6,rep(5,6)),Y=dyes)
dyes$Batch <- as.factor(dyes$Batch)
dyes

#######################################

# Take a quick look at the data:

par(mfrow=c(1,1))
boxplot(split(dyes$Y,dyes$Batch))
points(dyes)

# It's not entirely clear from this plot whether "between" variation
# or "within" variation will win.

# here is a fancier way to make a similar plot:

library(lattice)
# try help(lattice), help(dotplot), help(panel.dotplot), help(panel)...

dotplot(Y ~ 1|Batch,data=dyes,horizontal=F)

bwplot(Y ~ 1|Batch,data=dyes,horizontal=F)

bwplot(Y ~ 1|Batch,data=dyes,horizontal=F,
       panel=function(...) {
         panel.dotplot(...)
         par(new=T)
         panel.bwplot(...)
         par(new=F)
       }
       )

# One way to deal with this is with a simple anova model
#
# y[i,j] = b0 + u[i] + e[i,j]
#
# where
#   b0 is a fixed intercept
#   u[i] are fixed contrasts from b0 (so they sum to zero)
#   e[i,j] ~ N(0,s^2), iid, are random draws for y[i,j]
#          deviations from u[i] + b0
#
# We are estimating 7 parameters: 5 u's (6 minus 1 constraint),
# b0, and s^2.
#
# Note also that this model is equivalent to
#
# y[i,j] ~ N(b0 + u[i], s^2), indep but not identically distributed.
#

summary(aov1 <- lm(Y ~ Batch,data=dyes,contrasts=list(Batch="contr.sum")))

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1527.50       9.04 168.985  < 2e-16 ***
# Batch1        -22.50      20.21  -1.113  0.27666    
# Batch2          0.50      20.21   0.025  0.98047    
# Batch3         36.50      20.21   1.806  0.08351 .  
# Batch4        -29.50      20.21  -1.459  0.15739    
# Batch5         72.50      20.21   3.587  0.00149 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 49.51 on 24 degrees of freedom
# Multiple R-Squared: 0.4893,     Adjusted R-squared: 0.3829 
# F-statistic: 4.598 on 5 and 24 DF,  p-value: 0.004398 

par(mfrow=c(2,2))
plot(aov1,1:4)

# To some extent there is conflicting evidence here:  The F test
# likes Batch, suggesting that between variance is dominating over
# within.  One could check this by using aov() instead of lm(), e.g.
#
# However, most of the Batch coefficients are nonsignificant,
# suggesting batch means that do not differ significantly from the
# grand mean, and R^2 is not enormous.
#
# [the coeff for Batch6 is -sum(aov1$coef[-1]) = -57.5]
#
# The evidence from the table of estimated coefficients is
# basically that 7 parameters is too much to model the
# between-batch variation here. 

# We could treat this as a kind of collinearity problem and knock u[i]'s
# out of the model. However, if we want to keep the idea that there
# are different means for each batch, an alternative approach is to
# fit a "mixed effects" linear model:
#
# y[i,j] = b0 + u[i] + e[i,j]
#
# where:
#   b0 is a fixed intercept
#   u[i] ~ N(0,s^2_between), iid, is a random draw for batch
#          mean i's deviations from b0
#   e[i,j] ~ N(0,s^2_within), iid, is a random draw for y[i,j]
#          deviations from u[i] + b0
#
# Note that the structure of the model is the same, but now we are
# estimating only 3 parameters: b0, s^2_between, and s^2_within.
#
# The b0 is a "fixed effect"; the u[i] together are one "random effect",
# and s^2_between, and s^2_within are called "variance components"
#
# Indeed this model is equivalent to
#
# y[i,j] ~ N(b0, s^2_between + s^2_within)
#
# BUT the y[i,j] are no longer independent of each other: y[i,j] in the
# same batch i depend on the same random draw u[i] and so are dependent.
#
# In addition, you can now see why s^2_between, s^2_within are called
# "variance components".
#

# R has two packages that can fit this sort of model.  The first, "lme",
# is older, more stable, more flexible, and more complicated to use.
#
library(nlme)

summary(lme1 <- lme(Y ~ 1, random = ~ 1|Batch, data=dyes))

# Linear mixed-effects model fit by REML
#  Data: dyes 
#        AIC      BIC    logLik
#   325.6543 329.7562 -159.8271
# 
# Random effects:
#  Formula: ~1 | Batch
#         (Intercept) Residual
# StdDev:    42.00061  49.5101
# 
# Fixed effects: Y ~ 1 
#              Value Std.Error DF  t-value p-value
# (Intercept) 1527.5  19.38342 24 78.80448       0
# 
# Standardized Within-Group Residuals:
#       Min         Q1        Med         Q3        Max 
# -1.4116948 -0.7633720  0.1418360  0.7791774  1.8296175 
# 
# Number of Observations: 30
# Number of Groups: 6 

#
# From this we learn that
#
# b0 = 1527.5 (19.38)
#
# s_within  = 42.00
# s_between = 49.5
#
# EXERCISE: Show that the correlation between any two observations
# in the SAME batch is 49.5^2/(42^2+49.5^2), whereas the correlation
# between observations in DIFFERENT batches is 0.
#
# This correlation is (for obvious reasons!) called the "intra-class
# correlation", or ICC.

# Some simple residual plots
#

par(mfrow=c(2,2))

plot(fitted(lme1,level=0),resid(lme1,type="r",level=0));abline(0,0)
plot(fitted(lme1,level=1),resid(lme1,type="r",level=1));abline(0,0)
plot(fitted(lme1,level=0),resid(lme1,type="p",level=0));abline(0,0)
plot(fitted(lme1,level=1),resid(lme1,type="p",level=1));abline(0,0)

par(mfrow=c(1,1))

# standardized residuals within batch
plot(lme1, resid(.) ~ fitted(.) | Batch, abline = 0)

# box-plots of residuals by Batch
plot(lme1,  resid(.) ~ as.numeric(Batch), horizontal=F,
            panel=function(...) {
         panel.dotplot(...)
         par(new=T)
         panel.bwplot(...)
         par(new=F)
       }
)

# observed versus fitted values by 
plot(lme1, Y ~ fitted(.) | Y, abline = c(0,1),horizontal=F)

######################################
#
# another way to fit the model is with "lmer".  "lmer" is a
# rewrite of "lme" with a simpler model formula language.
# It is less flexible in terms of specifying correlated
# and nested random effects, but it fits a broader range
# of models (linear mixed models AND generalized mixed linear
# models), with less user effort.
#
# You can specify a mixed model with a single formula in lmer,
# rather than separate formulas for the fixed and random
# effects, as in lme.  The general from of the formula is
#
# response ~ "fixed terms" + "random terms"
#
# where "fixed terms" are sums of the usual modeling formulae in R
# and "random terms" are sums of terms like this:
#
# ( "Z-var" | "grouping var" )
#
# The "Z-var" is what the random effect will be multiplied by (a
# column or columns of the Z matrix); and the "grouping var"
# should be (equivalent to) a factor that indicates which
# observations use the same "draw" from the random effect
# distribution.
#
# Thus for our linear mixed model
#
# y[i,j] = b0 + u[i] + e[i,j]
#
# the appropriate model formula is
#
# Y ~ 1 + (1|Batch)
#
# where the 1 in the model formula specifies the intercept b0
# and the (1|Batch) specifies u[i] (one draw of u for each
# level of Batch, and multiply the result by 1).

search()
detach("package:nlme") # to avoid conflicts between functions in
                       # the two packages...

library(lme4)

summary(lme2 <- lmer(Y ~ 1 + (1|Batch),data=dyes))
#
# Linear mixed-effects model fit by REML 
# Formula: Y ~ 1 + (1 | Batch) 
#    Data: dyes 
#    AIC   BIC logLik MLdeviance REMLdeviance
#  325.7 329.9 -159.8      327.4        319.7
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Batch    (Intercept) 1763.7   41.996  
#  Residual             2451.3   49.511  
# number of obs: 30, groups: Batch, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1527.50      19.38   78.81

#
# the results are the same as before, but the model language
# is a little simpler, and the output is a little cleaner

# Is the mixed effects model better or worse than the aov model??

# We can look at residuals again; they will be the same as before
# (but there aren't special plot methods for "lmer" objects,
# so we roll our own)...

#
# Some simple residual plots
#

par(mfrow=c(2,2))

plot(fitted(lme2,level=0),resid(lme2,type="r",level=0));abline(0,0)
plot(fitted(lme2,level=1),resid(lme2,type="r",level=1));abline(0,0)
plot(fitted(lme2,level=0),resid(lme2,type="p",level=0));abline(0,0)
plot(fitted(lme2,level=1),resid(lme2,type="p",level=1));abline(0,0)

par(mfrow=c(1,1))

# you can see that lmer only knows how to do level 1 raw residuals...

xyplot(resid(lme2) ~ fitted(lme2)|dyes$Batch)

xyplot(resid(lme2) ~ dyes$Batch, horizontal=F,
            panel=function(...) {
         panel.dotplot(...)
         par(new=T)
         panel.bwplot(...)
         par(new=F)
       }
)

# Which model actually fits better?

BIC(logLik(aov1))
# [1] 336.381
BIC(logLik(lme2))
# [1] 326.4567

AIC(logLik(aov1))
# [1] 326.5726
AIC(logLik(lme2))
# [1] 323.6543

# so, the linear mixed model fits better than the
# ordinary linear regression/aov model.

# Note: Even though the models are nested,
# direct LR comparisons are tricky here,
# because you get the aov model by putting
# s^2_within at the EDGE of its parameter
# space...

# Just for fun,

logLik(aov1)
# 'log Lik.' -156.2863 (df=7)

logLik(lme2)
# 'log Lik.' -159.8271 (df=3)
#
# This leads to  an LR test statistic of about 7
# on 4 df which if we believe LR chisq here is not
# significant, so again the random effects model "wins".

# Now let's extract the between and within variance and compare
# them:
#

slotNames(summary(lme2))
# slots and slotNames() are like list elements and names()
# in older R objects. I am not sure why the switch from
# list objects to slotted objects was done by the R dev.
# team...
#
# Note "@" to extract slots, just like "$" to extract
# list elements...

summary(lme2)@REmat
 Groups     Name          Variance Std.Dev.
 "Batch"    "(Intercept)" "1763.9" "41.999"
 "Residual" ""            "2451.3" "49.510"
vc <- as.numeric(summary(lme2)@REmat[,3])
names(vc)=c("Within","Between")
vc
# Within Between 
# 1763.9  2451.3 



# we could formally compare models to see if the
# within variance component is nonzero...

lme0 <- lmer(Y ~ 1, data=dyes) # no var comp, so no lmer...

logLik(lme2)
# 'log Lik.' -159.8271 (df=3)

logLik(lme0)
# 'log Lik.' -166.3649 (df=2)

# This looks like a significant difference, on one df.  But we compute
# BIC's by hand, "to be sure..."

c(-2*logLik(lme2) + 3*log(dim(dyes)[1]))
# [1] 329.8579

c(-2*logLik(lme0) + 2*log(dim(dyes)[1]))
# [1] 339.5323

# Or we can use the  BIC() function ...

BIC(logLik(lme2))
# [1] 329.8579

BIC(logLik(lme0))
# [1] 339.5323

# Usual rules for comparing BIC's:
#
# a difference of 3 is significant evidence in favor
# of the model with smaller BIC
#
# a difference of 10 is very big evidence in favor of
# the model with smaller BIC

# So...
# By the usual BIC rules of thumb, a difference of 10 is a
# big deal, so this is evidence in favor of including the
# "within" variance component.

detach("package:lme4")

#################################################################

############################################
#
# Let's try another example...

############################################
#
# Dalgaard (2006) discusses an example in which
# bone mineral density (BMD) in the spine is
# measured at 0, 3, 6, 9, 12, 18 and 24 months
#
# The variables are:
#
# grp - a grouping variable; we can think of
#       this as tx (1) vs ctrl (2)
#
# time - which observation time we are considering
#
# time2 - converted to months, so time2=(time-1)*3
#
# spinebmd - spinal bone mineral density
#
# id - subject id (useful for specifying some r.e.'s)


spi <- read.csv("spinebmd.csv")
spi

spiLong <- reshape(spi, varying=list(names(spi)[2:8]),
                   v.names="spinebmd",direction="long")
spiLong$time2 <- c(0,3,6,9,12,18,24)[spiLong$time]
spiLong$grp <- factor(spiLong$grp)
spiLong

# reshape does automagically what I did by hand for the
# 'dyes' data above...

library(lattice)

xyplot(spinebmd~time2|id, groups=grp, type="b",
       data=na.omit(spiLong))

# First we try lme again...

library(nlme)

lme1 <- lme(fixed=spinebmd~time2+grp:time2, random=~time2|id,
          data=na.omit(spiLong))

# we will look at summaries of the fitted model later; for
# now, here are some "easy" exploratory plots for model fit...

plot(augPred(lme1,primary="time2"), grid=T)

plot(lme1, resid(.) ~ time2 | id, abline = c(0,0),horizontal=F)

#
# The model is basically:
#
# y[i,j,t] = (b0+u0[i]) + (b1 + b2[j] + u1[i])*time[t] + error[i,j,t]
#
# for subject i, group j, time t
#
# thus, each person gets his/her own growth curve, plus an
# effect on slope for group
#
# an interesting question is, is there an effec for tx?  we take
# the grp:time2 term out of the model, and check.

lme2 <- lme(fixed=spinebmd ~ time2, random=~time2|id,
                    data=na.omit(spiLong))

anova(lme1,lme2)

#      Model df       AIC       BIC   logLik   Test   L.Ratio p-value
# lme1     1  7 -736.4602 -711.2196 375.2301                         
# lme2     2  6 -738.6786 -717.0217 375.3393 1 vs 2 0.2183752  0.6403
# Warning message:
# Fitted objects with different fixed effects. REML comparisons are
# not meaningful

# This is a problem with REML.  Once it was favored because it was
# faster.  But the theory for model comparison wants you to be at
# the ML...

lme1 <-  lme(fixed=spinebmd ~ time2 + grp:time2, random=~time2|id,
                    data=na.omit(spiLong),method="ML")

lme2 <-  lme(fixed=spinebmd ~ time2 , random=~time2|id,
                    data=na.omit(spiLong),method="ML")

anova(lme1,lme2)

#      Model df       AIC       BIC   logLik   Test  L.Ratio p-value
# lme1     1  7 -768.1174 -742.8000 391.0587                        
# lme2     2  6 -757.6781 -735.9775 384.8390 1 vs 2 12.43928   4e-04

# note the big difference in model comparison! REML does *NOT*
# yield the same answers as ML!

# We conclude that the the tx effect on slope matters, even
# when each person has his/her own personal growth curve (line)
# for bone mineral density.  These individual regressions can
# be inspected with

lme1$coef

# lme() produces a list object; lmer() produces a slotted object....
#
# you would add the fixed and random eff estimates to get each
# person's individual linear growth curve...

#####################

# now, let's try this with lmer:

detach("package:nlme")
library("lme4")


summary(lme1 <- lmer(spinebmd ~ time2 + grp:time2 + (time2|id),
                    data=na.omit(spiLong)))
 
# Linear mixed-effects model fit by REML 
# Formula: spinebmd ~ time2 + grp:time2 + (time2 | id) 
#    Data: na.omit(spiLong) 
#     AIC    BIC logLik MLdeviance REMLdeviance
#  -738.5 -716.8  375.2     -782.1       -750.5
# Random effects:
#  Groups   Name        Variance   Std.Dev.  Corr   
#  id       (Intercept) 2.7351e-02 0.1653808        
#           time2       3.5423e-06 0.0018821 -0.342 
#  Residual             1.5164e-03 0.0389416        
# number of obs: 275, groups: id, 43
# 
# Fixed effects:
#               Estimate Std. Error t value
# (Intercept)  1.0398146  0.0255176   40.75
# time2       -0.0018531  0.0005781   -3.21
# time2:grp2   0.0030952  0.0008275    3.74
# 
# Correlation of Fixed Effects:
#            (Intr) time2 
# time2      -0.236       
# time2:grp2  0.000 -0.660


#
# Just as before, the model is basically:
#
# y[i,j,t] = (b0+u0[i]) + (b1 + b2[j] + u1[i])*time[t] + error[i,j,t]
#
# for subject i, group j, time t
#
# thus, each person gets his/her own growth curve, plus an
# effect on slope for group
#
# The fixed effect estimates are
#
# b0    =  1.04
# b1    = -0.002
# b2[1] =  0
# b2[2] =  0.003
#
# The variance components (or actually their square roots) are
#
# s_u0  = 0.165
# s_u1  = 0.002
# s_err = 0.039


# Note that including (time2|id) included both a slope random effect
# grouped within each person (id) and an intercept random effect
# grouped within each person.

# If we only wanted the slope random effect, we could take out the
# intercept random effect, by analogy with taking out an intercept
# in a linear regression model:
#
# lmer(spinebmd ~ time2 + grp:time2 + (time2-1|id),
#                    data=na.omit(spiLong)))
#


summary(lme2 <- lmer(spinebmd ~ time2 + (time2|id),
                    data=na.omit(spiLong)))

# Note that lmer again "defaults" to REML estimation.  Let's try
# comparing lme1 and lme2, with their default REML estimates:

anova(lme1,lme2)

# Data: na.omit
# Data: spiLong
# Models:
# lme2: spinebmd ~ time2 + (time2 | id)
# lme1: spinebmd ~ time2 + grp:time2 + (time2 | id)
#      Df     AIC     BIC  logLik  Chisq Chi Df Pr(>Chisq)    
# lme2  5 -759.66 -741.58  384.83                             
# lme1  6 -770.08 -748.38  391.04 12.418      1  0.0004252 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#
# Note that even though the original models were fitted with REML,
# the anova method for "lmer" refitted using ML before doing the
# comparison...

#
# This only scratches the surface of what's possible.
#
# More extensive examples, geared to lme(), are given in
# fox-appendix-mixed-models.pdf, an online appendix to
# John Fox's "Applied Linear Models" text.
#
# Similar examples could be constructed for lmer()...





