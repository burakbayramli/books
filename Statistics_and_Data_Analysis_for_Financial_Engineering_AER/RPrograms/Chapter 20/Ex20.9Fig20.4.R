#  Example 20.9 and Figure 20.4

library(R2WinBUGS)
N = 500
set.seed(19485)
y = rt(N,5)
y = 3 + 2*y
2 * sqrt(5/3)
data=list("y","N")
inits=function(){list(mu=runif(1,.1,5),tau=runif(1,.1,5),df=runif(1,2.5,15))}

univt.sim = bugs(data,inits,model.file="univt.bug",
   parameters=c("mu","tau","df","lambda"),n.chains = 5,n.iter=3000,
   n.burnin=1000,n.thin=1,debug=FALSE,
   bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,
   bugs.seed=NULL)

print(univt.sim,digits=2)
mu = as.vector(univt.sim$sims.array[,,1])
tau = as.vector(univt.sim$sims.array[,,2])
sigma = 1 / sqrt(tau)
k = as.vector(univt.sim$sims.array[,,3])
dev1 = univt.sim$sims.array[,1,4]
corrmatrix = cor(univt.sim$sims.array[,1,1:3])

postscript('univnorm_kde.ps',width=6,height=5)  #  Figure 20.4
par(mfrow=c(2,2))
plot(density(mu),main="mu")
plot(density(sigma),main="sigma")
plot(density(k),main="df")
plot(density(tau),main="tau")
graphics.off()

postscript('univnorm_trace.ps',width=6,height=5)  #  Not used in book
par(mfrow=c(2,2))
ts.plot(mu,xlab="iteration",ylab="",main="mu")
ts.plot(sigma,xlab="iteration",ylab="",main="sigma")
ts.plot(k,xlab="iteration",ylab="",main="df")
ts.plot(tau,xlab="iteration",ylab="",main="tau")
graphics.off()

postscript('univnorm_acf.ps',width=6,height=5)  #  Not used in book
par(mfrow=c(2,2))
acf(mu,main="mu")
acf(sigma,main="sigma")
acf(k,main="df")
acf(tau,main="tau")
graphics.off()














