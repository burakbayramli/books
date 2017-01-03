#  Example 19.7 and Figures 19.4 and 19.5

data(SP500,package="Ecdat")
library("fGarch")
# daily observations from 1981–01 to 1991–04 
# number of observations : 2783 
# daily return S&P500 (change in log index) 
n0 = 2783
SPreturn = SP500$r500[(n0-999):n0]
year = 1981 + (1:n0)* (1991.25-1981)/n0
year = year[(n0-999):n0]
n = length(SPreturn)
x=sort(SPreturn)

postscript("SP500_tail_plots.ps",width=6,height=5)  #  Figure 19.4
par(mfrow=c(2,2))
m = 50
xx = log((1:m)/n)
yy = log(-x[1:m])
plot(xx ,yy,  xlab="log(k/n)",
  ylab=expression(paste("log(-",R[(k)],")" )),main="(a) m=50")
fit = lm(yy~xx)
text(-4,-2,paste("slope = ",round(fit$coef[2],3)))
text(-4,-2.25,paste("a = ",round(-1/fit$coef[2],3)))
abline(fit,lwd=2)

m = 100
xx = log((1:m)/n)
yy = log(-x[1:m])
plot(xx ,yy,  xlab="log(k/n)",
  ylab=expression(paste("log(-",R[(k)],")" )),main="(b) m=100")
fit = lm(yy~xx)
text(-3.5,-2,paste("slope = ",round(fit$coef[2],3)))
text(-3.5,-2.35,paste("a = ",round(-1/fit$coef[2],3)))
abline(fit,lwd=2)

m = 200
xx = log((1:m)/n)
yy = log(-x[1:m])
plot(xx ,yy,  xlab="log(k/n)",
  ylab=expression(paste("log(-",R[(k)],")" )),main="(c) m=200")
fit = lm(yy~xx)
text(-2.8,-2,paste("slope = ",round(fit$coef[2],3)))
text(-2.8,-2.35,paste("a = ",round(-1/fit$coef[2],3)))
abline(fit,lwd=2)

m = 300
xx = log((1:m)/n)
yy = log(-x[1:m])
plot(xx ,yy,  xlab="log(k/n)",
  ylab=expression(paste("log(-",R[(k)],")" )),main="(d) m=300")
fit = lm(yy~xx)
text(-3,-2,paste("slope = ",round(fit$coef[2],3)))
text(-3,-2.45,paste("a = ",round(-1/fit$coef[2],3)))
abline(fit,lwd=2)

graphics.off()

q = quantile(x,.1)
print(q,digits=3)
print(-20000*q,digits=3)
a= 1.975
print(1/a,digits=4)
fitt = fitdistr(SPreturn,"t")
param = as.numeric(fitt$estimate)
mean = param[1]
df = param[3]
sd = param[2]*sqrt( (df)/(df-2) )
lambda = param[2]
alpha = seq(.002,.2,.0001)
qalpha = qt(alpha,df=df)
VaR_par = -20000*(mean + lambda*qalpha)
VaR_norm = -20000*(mean(x)+ sd(x)* qnorm(alpha))

postscript("var_sp500_est.ps",width=6,height=5) #  Figure 19.5
plot(alpha, -20000*q * (.1/alpha)^(1/a),type="l",lwd=2,
  xlab=expression(alpha),ylab=expression(paste("VaR(",alpha,")")),
  ylim=c(0,1700),log="x" )
a.hill=2.2
lines(alpha, -20000*q * (.1/alpha)^(1/a.hill),lty=2,lwd=2)
lines(alpha,VaR_par,lty=5,lwd=2)
lines(alpha,VaR_norm,lty=3,lwd=2)
legend("topright",c("polynomial tail: regression",
"polynomial taill: Hill","t","normal"),lwd=2,lty=c(1,2,5,3))
graphics.off()















