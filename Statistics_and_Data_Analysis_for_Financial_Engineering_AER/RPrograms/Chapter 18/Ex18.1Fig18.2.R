#  Example 18.1 and Figure 18.2

set.seed("8855")
n = 110
e = rnorm(n)
a=e
u = e
sig2= e^2
omega = 1
alpha = .95
phi = .8
mu = .1

for (i in 2:n)
{
sig2[i+1] = omega + alpha * a[i]^2
a[i] = sqrt(sig2[i])*e[i]
u[i] = mu + phi*(u[i-1]-mu) + a[i]
}

postscript("garch01.ps",width=8,height=4)
par(mfrow=c(2,4))
plot(e[11:n],type="l",xlab="t",ylab=expression(epsilon),main="(a) white noise")
plot(sqrt(sig2[11:n]),type="l",xlab="t",ylab=expression(sigma[t]),
    main="(b) conditional std dev")
plot(a[11:n],type="l",xlab="t",ylab="a",main="(c) ARCH")
plot(u[11:n],type="l",xlab="t",ylab="u",main="(d) AR/ARCH")
acf(a[11:n],main="(e) ARCH")
acf(a[11:n]^2,main="(f) ARCH squared")
acf(u[11:n],main="(g) AR/ARCH")
acf(u[11:n]^2,main="(h) AR/ARCH squared")
graphics.off()


