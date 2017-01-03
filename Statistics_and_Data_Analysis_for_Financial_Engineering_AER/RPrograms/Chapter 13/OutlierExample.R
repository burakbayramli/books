set.seed(788)
n = 40 
k=5
x= seq(0,10,length=n)
sigma = 1/2
y=3 + 2*x + sigma*rnorm(n) 
y[k] = y[k] + 12 


fit1 = lm(y[-k]~x[-k])
fit2 = lm(y~x)
fit3 = mst.fit(X=cbind(rep(1,n),x),y=y,plot=F)
fitted3 = cbind(rep(1,n),x) %*% fit3$dp$beta
plot(x,y)
lines(x[-k],fitted(fit1))
lines(x,fitted(fit2),lty=2)
lines(x,fitted3,lty=3)
