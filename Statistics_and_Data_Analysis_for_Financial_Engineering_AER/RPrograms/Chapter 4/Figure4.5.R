
postscript("constructingKDE.ps",width = 6,height =12)  #  Figure 4.5
set.seed("992291")
x = rnorm(6)
l = matrix(1,200,6)
l2 = l
l3 =l
xgrid = seq(from=-3,to=1.5,length=200)
for (i in 1:6)
{
l[,i] = dnorm(xgrid,x[i],.4)/6
l2[,i] = dnorm(xgrid,x[i],.2)/6
l3[,i] = dnorm(xgrid,x[i],.8)/6

}
par(cex.axis=1.5,cex.lab=1.5,mfrow=c(2,1))
kde = l%*%matrix(1,6,1)
kde2 = l2%*%matrix(1,6,1)
kde3 = l2%*%matrix(1,6,1)
plot(xgrid,kde,type="l",lwd=3,xlab="x",
font.lab=1,
ylab="KDE",main="b = 0.4")
for (i in 1:6)
{
lines(xgrid,l[,i],lwd=2,lty=2)
}
rug(x,side=1,lwd=2)

plot(xgrid,kde2,type="l",lwd=3,xlab="x",ylab="KDE",main="b = 0.2")
for (i in 1:6)
{
lines(xgrid,l2[,i],lwd=2,lty=2)
}
rug(x,side=1,lwd=2)
graphics.off()
