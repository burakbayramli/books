
x = seq(0,1,.01)

postscript("beta_densities2.ps",width=6,height=5)
par(lwd=2)
plot(x,dbeta(x,500,500),type="l",lty=1,xlab="y",
   ylab="density",main="beta densities")
lines(x,dbeta(x,20,20),type="l",lty=2)
lines(x,dbeta(x,3,3),type="l",lty=3)

legend("topleft",c(
   expression(paste(alpha," = ",beta," = 500")) ,
   expression(paste(alpha," = ",beta," = 20")),
   expression(paste(alpha," = ",beta," = 3"))),
   lty=c(1,2,3))
graphics.off()