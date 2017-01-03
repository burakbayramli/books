#  Figure 19.8

sigma = 2
sigma2 = sqrt(2)
x= seq(-120,2005,length=2000)
y = .04*pnorm(x,mean=2000,sd=sigma) + .96*pnorm(x,mean=-100,sd=sigma)

postscript("nearlyDiscrete.ps",width=6,height=3.5)  #  Figure 19.8
par(mfrow=c(1,2))
plot(x,y,type="l",ylim=c(.9,1),xlab="loss",ylab="prob",main="Portfolio 1")
abline(h=.95,lty=2)
xspline = seq(-120,-90,length=2000)
yspline = .04*pnorm(xspline,mean=2000,sd=sigma) + 
   .96*pnorm(xspline,mean=-100,sd=sigma)
VaR1 = spline(yspline,xspline,xout=.95)
y1 = (.04^2)*pnorm(x,mean=2000,sd=sigma2) 
y2= (2*.04*.96)*pnorm(x,mean=950,sd=sigma2)
y3 = (.96^2) * pnorm(x,mean=-100,sd=sigma2)
y = y1+y2+y3
plot(x,y,type="l",ylim=c(.9,1),xlab="loss",ylab="prob",main="Portfolio 2")
abline(h=.95,lty=2)
graphics.off()


xspline2 = seq(940,960,length=2000)
yspline1 = (.04^2)*pnorm(xspline2,mean=2000,sd=sigma2) 
yspline2= (2*.04*.96)*pnorm(xspline2,mean=950,sd=sigma2)
yspline3 = (.96^2) * pnorm(xspline2,mean=-100,sd=sigma2)
yspline4 = yspline1+yspline2+yspline3
options(digits=7)
VaR2 = spline(yspline4,xspline2,xout=.95,method="natural")

VaR1
VaR2



