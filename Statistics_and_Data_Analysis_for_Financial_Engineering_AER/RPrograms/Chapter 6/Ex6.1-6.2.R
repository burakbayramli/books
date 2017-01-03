#  Examples 6.1 and 6.2 and Figure 6.1
#  This takes awhile.  Try reducing "nboot" if you are impatient
#  Because of random variation in bootstrap results, the plot will not
#  look exactly like Figure 6.1 in the book.

library(bootstrap) 
library(MASS)    #  For fitdistr
data(CRSPday,package="Ecdat")
ge = CRSPday[,4]
ge_250 = ge[1:250]
nboot = 1000
options(digits=5)

tmle = function(x){as.vector(fitdistr(x,"t")$estimate)}

results = bootstrap(ge,nboot,tmle)
results_250 = bootstrap(ge_250,nboot,tmle)

apply(results$thetastar[,],1,mean)
apply(results$thetastar[,],1,sd)

apply(results_250$thetastar,1,mean)
apply(results_250$thetastar,1,sd)

fitdistr(ge,"t")
fitdistr(ge_250,"t")

postscript("MLE_t_BS_250.ps",width=7,height=3.5) # Figure 6.1
par(mfrow=c(1,2))
plot(density(results_250$thetastar[3,]),xlab="df",
  xlim=c(2,21),main="(a) n = 250")

plot(density(results$thetastar[3,]),xlab="df",
   xlim=c(2,21),main="(b) n = 2528")

graphics.off()







