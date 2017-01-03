library(forecast)
library(tseries)
library(urca)
yieldDat = read.table("treasury_yields.txt",header=T)
dat = as.matrix(yieldDat[,3:7])
year = seq(1990,2008-1/4,length=4714)

res = residuals(lm(dat[,3]~dat[,1]+dat[,2]+dat[,4]+dat[,4]))
postscript("yieldsCointegration.ps",width=6,height=4)
par(mfrow=c(2,4))
plot(year,dat[,1],type="l",ylab="",main="3 month")
plot(year,dat[,2],type="l",ylab="",main="6 month")
plot(year,dat[,3],type="l",ylab="",main="1 year")
plot(year,dat[,4],type="l",ylab="",main="2 year")
plot(year,dat[,5],type="l",ylab="",main="3 year")
plot(year,res,type="l",ylab="",main="residuals")
acf(res)
graphics.off()

po.test(dat[,c(3,1,2,4,5)])


options(digits=3)
summary(ca.jo(dat))


