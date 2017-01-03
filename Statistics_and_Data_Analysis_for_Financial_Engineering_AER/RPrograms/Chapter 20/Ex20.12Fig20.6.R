#  Example 20.12 and Figure 20.6

library("fEcofin")
library("R2WinBUGS")
x = midcapD.ts 
market = 100*as.matrix(x[,22])
x = 100*as.matrix(x[,-c(1,22)])

m=20
k=100

x1 = x[1:k,]
x2 = x[(k+1):500,]
mu1 = apply(x1,2,mean)
mu2 = apply(x2,2,mean)
means = apply(x1,2,mean)
sd2 = apply(x1,2,sd)
tau_mu = 1/mean(sd2^2)
tau_eps = 1/sd(means)^2

capm1=rep(0,m)
capm2=capm1
for (i in 1:m){
capm1[i] = mean(market[1:k])* as.numeric(lm(x1[,i]~-1+market[1:k])$coeff)
capm2[i] = mean(market[(k+1):500])* as.numeric(lm(x1[,i]~-1+market[1:k])$coeff)
}

n=k
data=list("x1","n","m")
inits=function(){list(alpha=.001,mu=means,tau_eps=tau_eps,tau_mu=tau_mu)}

means.mcmc = bugs(data,inits,model.file="midCap.bug",
   parameters=c("mu","tau_mu","tau_eps","alpha","sigma_mu","sigma_eps"),
   n.chains = 3,n.iter=5100,
   n.burnin=100,n.thin=1,
   bugs.directory="c:/Program Files/WinBUGS14/",codaPkg=FALSE,
   bugs.seed=19777)
print(means.mcmc,digits=3)
post.means = means.sim$summary[1:20]

delta = 5.4/(5.4+78.6)

postscript("midcap.ps",width=6,height=3.75) #  Figure 20.6
par(mfrow=c(1,2))
plot(c(rep(1,m),rep(2,m)),c(mu1,mu2),
   xlab="estimate                         target",ylab="mean",
   main="sample means",
   ylim=c(-.4,.8),axes=F)
axis(2)
axis(1,labels=F,tick=T,lwd.tick=0)
for (i in 1:m){ lines(1:2,c(mu1[i],mu2[i]),col=1) }

plot(c(rep(1,m),rep(2,m)),c(post.means,mu2),
   xlab="estimate                         target",ylab="mean",
   main="Bayes",
   ylim=c(-.4,.8),axes=F)
axis(2)
axis(1,labels=F,tick=T,lwd.tick=0)
for (i in 1:m){ lines(1:2,c(post.means[i],mu2[i]),col=1) }
graphics.off()

options(digits=2)
sum((mu1-mu2)^2 )
sum((post.means-mu2)^2)
sum((mean(mu1)-mu2)^2)
sum((capm1-mu2)^2)
sum((capm2-mu2)^2)





