#  Figure 21.4

x= seq(0,4,length=401)
plus = (x>2)*(x-2)
f = .5 + .2*x + .5*plus
f2 = (x>1)*(x-1)

postscript("splinePlots.ps",width=8,height=4)  #  Figure 21.4
par(mfrow=c(1,2))
plot(x,f,type="l",lwd=2,main="(a) Linear spline",ylab="")
plot(x,f2,type="l",lwd=2,main="(b) Linear plus function",ylab="")
lines(x,(x>1),lty=2,lwd=2)
legend("topleft",c("plus fn.","derivative"),lty=c(1,2),
   lwd=2)
graphics.off()



