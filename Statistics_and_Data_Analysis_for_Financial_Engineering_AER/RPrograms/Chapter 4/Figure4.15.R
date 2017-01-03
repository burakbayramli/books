data(SP500,package="Ecdat")
SPreturn = SP500$r500
n = length(SPreturn)

postscript("SP_qqnorm.ps",width=5,height=7)  ##  Figure 4.15
par(mfrow=c(3,2))
qqnorm(SPreturn,datax=TRUE,ylab="Data",xlab="normal quantiles",
main="(a) Normal probability plot",cex.lab=1,cex.axis=1,cex.main=1)
qqline(SPreturn,datax=TRUE)
s_SPreturn = sort(SPreturn)
grid = (1:n)/(n+1)
qqplot(s_SPreturn, qt(grid,df=1),main="(b) t-probability plot, 
df = 1",xlab="Data",ylab="t-quantiles",cex.lab=1,cex.axis=1,cex.main=1)
lmfit = lm( qt(c(.25,.75),df = 1) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=2),main="(c) t-probability plot, 
df = 2",xlab="Data",ylab="t-quantiles",cex.lab=1,cex.axis=1,cex.main=1)
lmfit = lm( qt(c(.25,.75),df = 2) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=4),main="(d) t-probability plot, 
df = 4",xlab="Data",ylab="t-quantiles",cex.lab=1,cex.axis=1,cex.main=1)
lmfit = lm( qt(c(.25,.75),df = 4) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=8),main="(e) t-probability plot, 
df = 8",xlab="Data",ylab="t-quantiles",cex.lab=1,cex.axis=1,cex.main=1)
lmfit = lm( qt(c(.25,.75),df = 8) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=15),main="(f) t-probability plot, 
df = 15",xlab="Data",ylab="t-quantiles",cex.lab=1,cex.axis=1,cex.main=1)
lines(quantile(s_SPreturn,c(.25,.75)),qt(c(.25,.75),df=15))
lmfit = lm( qt(c(.25,.75),df = 15) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

graphics.off()
