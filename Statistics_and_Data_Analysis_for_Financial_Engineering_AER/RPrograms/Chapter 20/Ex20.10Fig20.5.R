#  Example 20.10 and Figure 20.5


library(R2WinBUGS)
library(mnormt)
set.seed(90201)
N = 50
beta1 = 1
beta2=2
alpha = 1
x1 = rnorm(N,mean=3,sd=2)
x2 = x1 + rnorm(N,mean=3,sd=.2)
cor(x1,x2)
x=cbind(rep(1,N),x1,x2)
y = alpha + beta1*x1 + beta2*x2 + rnorm(N,mean=0,sd=.2)
data=list("y","x","N")
summ = summary(lm(y~x1+x2))
betahat = as.numeric( summ$coeff )[1:3]
covbetahat = summ$sigma^2 * solve(t(x)%*%x)

inits=function(){list(beta=as.numeric(rmnorm(n = 1, mean = betahat, 
  varcov=1.5*covbetahat)) ,
  tau=runif(1,1/(4*summ$sigma^2),4/summ$sigma^2))}

regr.sim = bugs(data,inits,model.file="lin_reg_vect.bug",
parameters=c("beta","tau"),n.chains = 3,n.iter=1100,n.burnin=100,n.thin=1,
bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,bugs.seed=9020)
regr.sim.largeN = bugs(data,inits,model.file="lin_reg_vect.bug",
parameters=c("beta","tau"),n.chains = 3,n.iter=35000,n.burnin=5000,n.thin=10,
bugs.directory="c:/Program Files/WinBUGS14/",debug=FALSE,
codaPkg=FALSE,bugs.seed=9020)

print(regr.sim.largeN)

postscript("linRegMCMC.ps",width=6,height=3)
par(mfrow=c(1,3))
plot(regr.sim$sims.array[,1,2],type="l",ylim=c(0.55,1.16),
   ylab=expression(beta[1]),xlab="iteration",main="(a)")
lines(regr.sim$sims.array[,2,2])
lines(regr.sim$sims.array[,3,2])


#  no collinearity

set.seed(90201)
N = 50
beta1 = 1
beta2=2
alpha = 1
x1 = rnorm(N,mean=3,sd=2)
x2 = rnorm(N,mean=3,sd=2) + rnorm(N,mean=3,sd=.2)
x=cbind(rep(1,N),x1,x2)
y = alpha + beta1*x1 + beta2*x2 + rnorm(N,mean=0,sd=.2)
data=list("y","x","N")
summ = summary(lm(y~x1+x2))
betahat = as.numeric( summ$coeff )[1:3]
covbetahat = summ$sigma^2 * solve(t(x)%*%x)

inits=function(){list(beta=as.numeric(rmnorm(n = 1, mean = betahat, varcov=1.5*covbetahat)) ,
  tau=runif(1,1/(4*summ$sigma^2),4/summ$sigma^2))}

regr.noco.sim = bugs(data,inits,model.file="lin_reg_vect.bug",
parameters=c("beta","tau"),n.chains = 3,n.iter=1100,n.burnin=100,n.thin=1,
bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,bugs.seed=9020)
print(regr.noco.sim)

plot(regr.noco.sim$sims.array[,1,2],type="l",ylim=c(.96,1.075),
   ylab=expression(beta[1]),xlab="iteration",main="(b)")
lines(regr.noco.sim$sims.array[,2,2])
lines(regr.noco.sim$sims.array[,3,2])

dim(regr.noco.sim$sims.array)


#######  larger N

set.seed(90201)
N = 50
beta1 = 1
beta2=2
alpha = 1
x1 = rnorm(N,mean=3,sd=2)
x2 = x1 + rnorm(N,mean=3,sd=.2)
cor(x1,x2)
x=cbind(rep(1,N),x1,x2)
y = alpha + beta1*x1 + beta2*x2 + rnorm(N,mean=0,sd=.2)
data=list("y","x","N")
summ = summary(lm(y~x1+x2))
betahat = as.numeric( summ$coeff )[1:3]
covbetahat = summ$sigma^2 * solve(t(x)%*%x)

inits=function(){list(beta=as.numeric(rmnorm(n = 1, mean = betahat, varcov=1.5*covbetahat)) ,
  tau=runif(1,1/(4*summ$sigma^2),4/summ$sigma^2))}

regr.sim.largeN = bugs(data,inits,model.file="lin_reg_vect.bug",
parameters=c("beta","tau"),n.chains = 3,n.iter=35000,n.burnin=5000,n.thin=10,
bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,bugs.seed=9020)
print(regr.sim.largeN)

plot(regr.sim.largeN$sims.array[,1,2],type="l",ylim=c(0.4,1.4),
   ylab=expression(beta[1]),xlab="iteration",main="(c)")
lines(regr.sim.largeN$sims.array[,2,2])
lines(regr.sim.largeN$sims.array[,3,2])
graphics.off()



