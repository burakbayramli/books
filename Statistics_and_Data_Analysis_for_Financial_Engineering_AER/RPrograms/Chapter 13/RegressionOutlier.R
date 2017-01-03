
set.seed("9847725")
x=seq(0,10,by=.4)
n = length(x)
y = 2 + 5*x + rnorm(n)
ind = c(3,24)
y[ind[1]] = y[ind[1]] + 35
y[ind[2]] = y[ind[2]] - 15

postscript("regression_outliers.ps",width=6,height=5)
plot(x[-ind],y[-ind],xlab="X",ylab="Y")
points(x[ind],y[ind],pch="*",cex=2.5)
abline(lm(y~x))
abline( lm(y[-ind]~x[-ind]),lty=5)
legend("bottomright",c("good data","residual outliers","with outliers",
   "w/o outliers"),
   pch=c("o","*",NA,NA),lty=c(NA,NA,1,5),pt.cex=c(1,2.5,1,1))
graphics.off()