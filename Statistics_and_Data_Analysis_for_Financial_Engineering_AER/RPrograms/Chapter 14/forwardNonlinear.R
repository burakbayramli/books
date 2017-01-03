dat = read.table("strips_dec95.txt",header=TRUE)
dat = dat[order(dat$T),]
attach(dat)
t = seq(0,30,length=100)
emp = -diff(log(dat$price))/diff(dat$T)

fitLin = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 ),data=dat,
     start=list(beta0=.03,beta1=0))
coefLin = summary(fitLin)$coef[,1]
forwardLin = coefLin[1] + coefLin[2]*t

fitQuad = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 ),data=dat,
     start=list(beta0=.03,beta1=0,beta2=0))
coefQuad = summary(fitQuad)$coef[,1]
forwardQuad = coefQuad[1] + (coefQuad[2]*t) + (coefQuad[3]*t^2) 

fitCubic = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 - (beta3*T^4)/4 ),data=dat,
     start=list(beta0=.03,beta1=0,beta2=0,beta3=0))
coefCubic = summary(fitCubic)$coef[,1]
forwardCubic = coefCubic[1] + (coefCubic[2]*t) + (coefCubic[3]*t^2) +(coefCubic[4]*t^3)

fitSpline = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 
     - (T>15)*(beta3*(T-15)^3)/3 ),data=dat,
     start=list(beta0=.03,beta1=0,beta2=0,beta3=0) )
coefSpline = summary(fitSpline)$coef[,1]
forwardSpline = coefSpline[1] + (coefSpline[2]*t)  + (coefSpline[3]*t^2)  + (t>15)*(coefSpline[4]*(t-15)^2)


fitSpline2 = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 
     - (T>10)*(beta3*(T-10)^3)/3 - (T>20)*(beta4*(T-20)^3)/3 ),data=dat,
     start=list(beta0=.03,beta1=0,beta2=0,beta3=0,beta4=0) )
coefSpline2 = summary(fitSpline2)$coef[,1]
forwardSpline2 = coefSpline2[1] + (coefSpline2[2]*t)  + (coefSpline2[3]*t^2)  + (t>10)*(coefSpline2[4]*(t-10)^3)/t +
   (t>20)*(coefSpline2[5]*(t-20)^2)


NS = function(b0,b1,b2,k)
{
ns_spot = b0 + (b1 + b2/k)*(1-exp(-k*T))/(k*T) - (b2/k)*exp(-k*T)
exp(-T*ns_spot)
}



b0=-.2
fitNS = nls(price ~ 100*exp(  -dat$T * (b0 + (b1 + b2/k)*(1-exp(-k*dat$T))/(k*dat$T) - (b2/k)*exp(-k*dat$T))  ),
    data=dat,start=list(b1=.017,b2=.004,k=.05),
    control = nls.control(maxiter=2500,minFactor = 1/1024),trace=T )
coefNS = summary(fitNS)$coef[,1]
b1 = coefNS[1]
b2 = coefNS[2]
k = coefNS[3]
NS_forward = b0 + (b1+b2*t)*exp(-k*t)

NSobj_fn = function(theta)
{
b0=theta[1]
b1=theta[2]
b2=theta[3]
k=theta[4]
pricehat = 100*exp(  -dat$T * (b0 + (b1 + b2/k)*(1-exp(-k*dat$T))/(k*dat$T) - (b2/k)*exp(-k*dat$T)))
sum((price - pricehat)^2)
}


fitNS = optim(c(-.2,.1,.1,.1),NSobj_fn)
theta = fitNS$par
b0=theta[1]
b1=theta[2]
b2=theta[3]
k=theta[4]
NS_forward = b0 + (b1+b2*t)*exp(-k*t)
NS_residual_error = sqrt(fitNS$value/(length(price)-4))




postscript("stripsNL01.ps",width=6,height=5)
plot(t,forwardQuad,type="l",ylim=c(0.025,.075),lwd=2,xlab="maturity",ylab="forward rate")
lines(t,forwardCubic,lty=3,lwd=3)
lines(t,forwardSpline,lty=2,lwd=2)
points(dat$T[2:length(dat$T)],emp,pch="*",cex=1.5)
legend("bottomleft",c("quadratic","cubic","spline","empirical"),
    lty=c(1,3,2,NA),lwd=c(2,3,2),pt.cex=1.5,pch=c(NA,NA,NA,"*"),cex=1.2 )
#lines(t,NS_forward,col="red")
graphics.off()

postscript("empirical.ps",width=8,height=4.5)
par(mfrow=c(1,2))
plot(dat$T,dat$price,xlab="maturity",ylab="price",main="(a)",cex=.75)
plot(dat$T[2:length(dat$T)],emp,,ylim=c(0.025,.075),lwd=2,xlab="maturity",
  ylab="empirical forward rate",type="b",cex=.75,
  main="(b)")
graphics.off()




