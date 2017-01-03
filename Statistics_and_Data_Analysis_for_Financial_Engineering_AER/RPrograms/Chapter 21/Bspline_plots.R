library("splines")
M = 20
K = 10
n = M*K
degree = 1
start = 1/n
x = seq(start,1,length.out=n)
x = seq(0,(1-start,length.out=n)
knots=seq(0,(1-1/K),length.out=(K))  # internal knots
c = .1
Bsp = bs(x,knots=knots,degree=degree)

plot(x,Bsp[,1],type="l",ylim=c(0,1),lwd=2,col=1)
for (i in 2:(dim(Bsp)[2]))
{
lines(x,Bsp[,i],col=(i+0),lwd=2)
}
Sigma_p = t(Bsp) %*% Bsp/M
J = rep(1,dim(Bsp)[2])
Sigma_p %*% J

ell = 8
sum(Bsp[,ell]) # sum ell-th basis function across the x's
sum(Bsp[ell,]) # sum across basis functions with x fixed at ell-th value


