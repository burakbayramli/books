# MLEs for several models used in Examples 5.4 to 5.7

# fitdistr() uses optim() which appears to be the best optimizer available in R
# stdFit(), sstdFit(), and gedFit() use the nlm optimizer which often fails (and may
#   fail here)

# I recommend using optim() for all MLE computations.  An example below shows how to
# fit the standardized t-model with optim [to avoid stdFit() and nlm()]

library(fGarch)
library(MASS)
data(Capm,package="Ecdat")  
diffrf=diff(Capm$rf)
n = length(diffrf)

fitdistr(diffrf,"t") # fits a t-model.  This function is in the MASS package

stdFit(diffrf) # fits a standardized t-model.  This function is in the fGarch package

sstdFit(diffrf) # fits a skewed t-model.  This function is in the fGarch package

gedFit(diffrf) # fits a ged model.  This function is in the fGarch package



start=c(mean(diffrf),sd(diffrf),4)
fit_std = optim(start,fn=function(theta)
  { -sum(log(dstd(diffrf,theta[1],theta[2],theta[3]))) },
   method="L-BFGS-B",lower=c(-1,.03,2.5) )
fit_std
