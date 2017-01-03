gibbs.fun = function(y,n,N){
     k     = length(y)
     p.hat = y/n
     Z     = log(p.hat/(1-p.hat))
     sigma = sqrt(1/(n*p.hat*(1-p.hat)))
     v     = 1/sqrt(k)
     mu    = rep(0,N)
     psi   = matrix(0,N,k)
     for(i in 2:N){
           ### draw mu given rest
           b     = mean(psi[i-1,])
           mu[i] = rnorm(1,b,v)

           ### draw psi given rest
           e       = ( (Z/sigma^2) + mu[i])/(1 + (1/sigma^2))
           d       = sqrt(1/(1 + (1/sigma^2)))
           psi[i,] = rnorm(k,e,d)
          }
     list(mu=mu,psi=psi)
     }


k = 40
p = rbeta(k,5,5)
n = rep(20,k)
y = rbinom(k,n,p)
print(y/n)

N = 1000

out = gibbs.fun(y,n,N)
postscript("gibbs1.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,1))
p  =  exp(out$psi)/(1+exp(out$psi))
mu = out$mu
plot(1:N,p[,1],type="l",xlab="",ylab="",xaxt="n",yaxt="n",bty="l",ylim=c(0,1))
axis(1,c(0,500,1000),cex.axis=2);axis(2,c(0,0.5,1.0),cex.axis=2)
plot(1:N,mu,type="l",xlab="",ylab="",xaxt="n",yaxt="n",bty="l",
     ylim=c(-.5,.5))
axis(1,c(0,500,1000),cex.axis=2);axis(2,c(-0.5,0,0.5),cex.axis=2)
dev.off()

postscript("gibbs2.eps")
par(mfrow=c(2,1))
hist(mu,xlab="",ylab="",main="",xaxt="n",yaxt="n",bty="n",density=6)
axis(1,c(-0.6,0,0.6),cex.axis=2)
p.bayes = apply(p,2,mean)
plot(y/n,rep(1,k),xlim=c(0,1),ylim=c(1,2),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="l")
points(p.bayes,rep(2,k))
for(i in 1:k){
     lines(c(y[i]/n[i],p.bayes[i]),c(1,2))
     }
axis(1,c(0,0.5,1.0),cex.axis=2)
dev.off()


