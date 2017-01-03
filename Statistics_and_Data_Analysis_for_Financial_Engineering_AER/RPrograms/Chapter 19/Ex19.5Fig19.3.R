#  Example 19.5 and Figure 19.3

data(SP500,package="Ecdat")
library("fGarch")
# daily observations from 1981–01 to 1991–04 
# number of observations : 2783 
# daily return S&P500 (change in log index) 
n = 2783
SPreturn = SP500$r500[(n-999):n]
length(SPreturn)
year = 1981 + (1:n)* (1991.25-1981)/n
year = year[(n-999):n]

#  parametric estimation
fitt = fitdistr(SPreturn,"t")
param = as.numeric(fitt$estimate)
mean = param[1]
df = param[3]
sd = param[2]*sqrt( (df)/(df-2) )
alpha = .05
n = length(SPreturn)
#  GARCH estimation
fit_garch = garchFit(~garch(1,1),SPreturn,cond.dist="std")
summary(fit_garch)
# plot(fit_garch)
pred = as.numeric(predict(fit_garch,n.ahead=1))
df = as.numeric(coef(fit_garch)[5])
q = qstd(alpha, mean = pred[1], 
   sd =pred[3], nu = df )
VaR = -20000*q
lambda = pred[3]/sqrt( (df)/(df-2) )
qalpha = qt(alpha,df=df)
es1 = dt(qalpha,df=df)/(alpha)
es2 = (df + qalpha^2) / (df - 1)
es3 = -mean+lambda*es1*es2
ES_par = 20000*es3
VaR
ES_par

postscript("SP500_GARCH_CondSD.ps",width=6,height=5)  #  Fig 19.3
plot(year,fit_garch@sigma.t,type="l",ylab="Cond SD")
points(year[1000]+1/365,pred[3],cex=4,pch="*")
abline(h=sd,lty=5)
legend("topright",c("conditional SD","marginal SD","next day's conditional SD"),
     lty=c(1,5,NA),pch=c(NA,NA,"*"),pt.cex=2.5)
graphics.off()


