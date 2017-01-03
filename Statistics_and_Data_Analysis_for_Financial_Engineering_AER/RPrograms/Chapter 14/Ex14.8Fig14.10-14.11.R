#  Example 14.8 and Figures 14.10 and 14.11

library("MASS")
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)

y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 


postscript("simDataBoxCox.ps",width=6,height=5)  #  Figure 14.10
boxcox(y~poly(x1,2)+x2,ylab="log-likelihood")
graphics.off()

yinv = -1/y
lm_bc = lm(yinv~poly(x1,2)+x2)
rstudent=rstudent(lm_bc)

postscript("simDataBoxCoxResid.ps",width=6,height=6)  #  Figure 14.11
par(mfrow=c(2,2))
plot(lm_bc$fitted,rstudent,xlab="fitted values",main="(a)")
plot(x1,rstudent,main="(b)",xlab=expression(x[1]))
plot(x2,rstudent,main="(c)",xlab=expression(x[2]))
qqnorm(rstudent,datax=T,main="(d)",xlab="theoretical quantiles",
   ylab="sample quantiles")
qqline(rstudent,datax=T)
graphics.off()