#  Figures 18.5 and 18.6

rho = function(a,b)
{
c(1,a*(1-a*b-b^2)/(1-2*a*b-b^2) * (a+b)^(0:9))
}

target = .5
a1 =.1
b1 = uniroot(f=function(b){rho(a1,b)[2] - target},interval = c(0,1-a1))$root
a2=.3
b2 = uniroot(f=function(b){rho(a2,b)[2] - target},interval = c(0,1-a2))$root
a3=.5
b3 = uniroot(f=function(b){rho(a3,b)[2] - target},interval = c(0,1-a3))$root

postscript("garch11ACF.ps",width=6,height=5)  #  Figure 18.5
plot(0:10,rho(a1,b1),type="b",ylim=c(0,1),lty=1,lwd=2,
    ylab=expression(paste(rho[a^2],"(lag)")),
    xlab="lag" )
lines(0:10,rho(a2,b2),type="b",lty=2,lwd=2)
lines(0:10,rho(a3,b3),type="b",lty=3,lwd=2)
legend("topright",c(expression(paste(alpha," = 0.10, ",beta," = 0.894")),
       expression(paste(alpha," = 0.30, ",beta," = 0.604")),
       expression(paste(alpha," = 0.50, ",beta," = 0.000")) ),lty=1:3,lwd=2)
graphics.off()

data(bmw,package="evir")
res = residuals(arima(bmw,order=c(1,0,0)))

postscript("bmwACFsquared.ps",width=6,height=5)  #  Figure 18.6
acf(res^2)
graphics.off()



