#  Figure 15.2


n = 5000
set.seed(12345)
a1 = .5
a2 = .55
lambda  = 1
y1 = rep(0,n)
y2 = y1
e1 = rnorm(n)
e2 = rnorm(n)
for (i in 2:n)
{
y1[i] = y1[i-1] + a1 * (y1[i-1] - lambda*y2[i-1]) + e1[i]
y2[i] = y2[i-1] + a2 * (y1[i-1] - lambda*y2[i-1]) + e2[i]
}

a1 - lambda*a2
ind = 10*(1:500)
postscript("cointSimEx1.ps",width=6,height=2.5)  #  Figure 15.2
par(mfrow=c(1,3))
plot(y1[ind],type="l",main=expression(Y[1]),ylab="")
plot(y2[ind],type="l",main=expression(Y[2]),ylab="")
plot(y1[ind]-lambda*y2[ind],type="l",
   main=expression(paste(Y[1],-lambda,Y[2])),ylab="")
graphics.off()