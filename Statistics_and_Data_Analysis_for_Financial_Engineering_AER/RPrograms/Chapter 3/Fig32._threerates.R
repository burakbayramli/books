# Program to create Figure 3.2

rates = as.matrix(read.table("WeeklyInterest.txt",header=F))
ff = rates[,4]/100
tb03 = rates[,5]/100
cm10 = rates[,6]/100
cm30 = rates[,7]/100
n=length(ff)
year = 1900 + rates[1:n,1:3] %*% c(1/12, 1/365 , 1) -1/12
postscript("threerates.ps",width=6,height=5,horizontal=F)
plot(year,tb03,type="l",xlim=c(1978,1994),lwd=2,ylab="rate",lty=2)
lines(year[cm30>0],cm10[cm30>0],lty=1,lwd=2,col="black")
lines(year[cm30>0],cm30[cm30>0],lty=3,lwd=2)
legend("topright",c("3-month","10-year","30-year"),lty=c(2,1,3),lwd=c(2,2,2),
   col=c("black","black","black"))
graphics.off()

