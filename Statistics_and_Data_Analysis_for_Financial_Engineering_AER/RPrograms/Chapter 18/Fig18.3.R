#  Figure 18.3

n = 200
set.seed("8855")
e = rnorm(n)
a=e
u = e
sig2= e^2
omega = 1
alpha = .08
beta = .9
phi = .8
mu = .1

for (i in 2:n)
{
sig2[i+1] = omega + alpha * a[i]^2 + beta*sig2[i]
a[i] = sqrt(sig2[i])*e[i]
u[i] = mu + phi*(u[i-1]-mu) + a[i]
}

postscript("garch03.ps",width=8,height=4)
par(mfrow=c(2,4))
plot(e[101:n],type="l",xlab="t",ylab=expression(epsilon),main="(a) white noise")
plot(sqrt(sig2[101:n]),type="l",xlab="t",ylab=expression(sigma[t]),
    main="(b) conditional std dev")
plot(a[101:n],type="l",xlab="t",ylab="a",main="(c) ARCH")
plot(u[101:n],type="l",xlab="t",ylab="u",main="(d) AR/ARCH")
acf(a[101:n],main="(e) GARCH")
acf(a[101:n]^2,main="(f) GARCH squared")
acf(u[101:n],main="(g) AR/GARCH")
acf(u[101:n]^2,main="(h) AR/GARCH squared")
graphics.off()


