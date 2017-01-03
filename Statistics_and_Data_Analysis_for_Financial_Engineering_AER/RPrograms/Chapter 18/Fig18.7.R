#  Figure 18.7

x = seq(-3,3,.005)
gamma = c(-.5,-.2,0,.12,.3,.9)

postscript("leverage_functions.ps",width=6,height=4)
par(mfrow=c(2,3))
for (i in 1:length(gamma))
{
gama = toString(gamma[i])
plot(x,abs(x) - gamma[i] * x,ylab=expression(paste(g[gamma],"(x)")),
   main=paste("gamma = ",gamma[i]),type="l",lwd=2  )  
}
graphics.off()