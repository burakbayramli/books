#  Figure 8.5

rho = seq(-1,1,by=.01)

df1=1
x1 = - sqrt((df1+1)*(1-rho)/(1+rho))
lambda1 = 2*pt(x1,df1+1)

df4=4
x4 = - sqrt((df4+1)*(1-rho)/(1+rho))
lambda4 = 2*pt(x4,df4+1)

df25=25
x25 = - sqrt((df25+1)*(1-rho)/(1+rho))
lambda25 = 2*pt(x25,df25+1)

df250=250
x250 = - sqrt((df250+1)*(1-rho)/(1+rho))
lambda250 = 2*pt(x250,df250+1)

postscript("TailDependenceT.ps",width=6,height=5)
par(lwd=2,cex.axis=1.2,cex.lab=1.2)
plot(rho,lambda1,type="l",lty=1,xlab=expression(rho),
   ylab=expression(lambda[l] == lambda[u]) )
lines(rho,lambda4,lty=2)
lines(rho,lambda25,lty=3)
lines(rho,lambda250,lty=4)
legend("topleft", c(expression(nu==1),expression(nu==4),expression(nu==25),
  expression(nu==250)),lty=1:4   )
graphics.off()



