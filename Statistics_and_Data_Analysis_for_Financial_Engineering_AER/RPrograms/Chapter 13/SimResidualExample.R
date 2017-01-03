#  SimResidualExample.R

library("SemiPar")
library(MASS)
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)

y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 

postscript("ResidEx5.ps",height=4,width=7)
par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))
graphics.off()

fit = lm(y~x1+x2)
rstudent = rstudent(fit)

postscript("ResidEx1.ps",height=4,width=7)
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot")
hist(rstudent,12)
graphics.off()

postscript("ResidEx3.ps",height=2.2,width=5)
par(mfrow=c(1,3))
plot(x1,rstudent,main="(a)",xlab=expression(x[1]))
fit2 = loess(rstudent~x1)
lines(x1,fit2$fitted)
plot(x2,rstudent,main="(b)",xlab=expression(x[1]))
fit3 = loess(rstudent~x2)
ordx2 = order(x2)
lines(x2[ordx2],fit3$fitted[ordx2])
fitquad = lm(y~poly(x1,2)+x2 )
rstudentquad = rstudent(fitquad)
plot(fitquad$fitted,abs(rstudentquad),xlab="fitted values",ylab="abs(rstudent)",main="(c) ")
fit4 = loess(abs(rstudentquad)~fitquad$fitted)
ord = order(fitquad$fitted)
lines(fitquad$fitted[ord],fit4$fitted[ord])
graphics.off()


transy = log(y)
fitquad2 = lm(transy~poly(x1,2)+x2 )
rstudentquad2 = rstudent(fitquad2)

postscript("ResidEx6.ps",height=5.5,width=6)
par(mfrow=c(2,2))
plot(x1,rstudentquad2,ylab="rstudent",main="(a)",xlab=expression(x[1]))
plot(x2,rstudentquad2,ylab="rstudent",main="(b)",xlab=expression(x[2]))
plot(fitquad$fitted,abs(rstudentquad2),xlab="fitted values",ylab="abs(rstudent)",main="(c) ")
fit5 = loess(abs(rstudentquad2)~fitquad2$fitted)
ord = order(fitquad2$fitted)
lines(fitquad2$fitted[ord],fit4$fitted[ord])
qqnorm(rstudentquad2,datax=T,main="(d) normal plot")
graphics.off()

summary(fitquad)

postscript("simDataBoxCox.ps",width=6,height=5)
boxcox(y~poly(x1,2)+x2,ylab="log-likelihood")
graphics.off()

yinv = -1/y
lm_bc = lm(yinv~poly(x1,2)+x2)

rstudent=rstudent(lm_bc)

postscript("simDataBoxCoxResid.ps",width=6,height=6)
par(mfrow=c(2,2))
plot(lm_bc$fitted,rstudent,xlab="fitted values",main="(a)")
plot(x1,rstudent,main="(b)",xlab=expression(x[1]))
plot(x2,rstudent,main="(c)",xlab=expression(x[2]))
qqnorm(rstudent,datax=T,main="(d)",xlab="theoretical quantiles",
   ylab="sample quantiles")
qqline(rstudent,datax=T)
graphics.off()










