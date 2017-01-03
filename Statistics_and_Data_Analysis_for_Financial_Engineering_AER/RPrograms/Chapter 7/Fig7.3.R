library(sn)
n = 2500
df= 3
set.seed(5600)

x = rmst(n,xi=rep(0,2),df=df,alpha=rep(0,2),Omega=diag(c(1,1)) )
y1 = rt(n,df=df)
y2 = rt(n,df=df)

postscript("MultiT_IndT.ps",width=8,height=4.5)  # Fig 7.3

par(mfrow=c(1,2))
plot(x[,1],x[,2],main="(a) Multivariate-t",
xlab=expression(X[1]),ylab=expression(X[2]))
abline(h=0)
abline(v=0)

plot(y1,y2,main="(b) Independent t",
xlab=expression(X[1]),ylab=expression(X[2]))
abline(h=0)
abline(v=0)

graphics.off()

