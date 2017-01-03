#  Figure 19.6

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
c = seq(quantile(SPreturn,.025),quantile(SPreturn,.25),by=.001)
nc = 0*c
den=nc
hill=nc
for (i in 1:length(nc))
{
ind =  (x<c[i]) 
nc[i] = sum(ind)
den[i] = sum(  log(x[ind]/c[i])  )
hill[i] = nc[i]/den[i]
}
postscript("var_sp500_hill.ps",width=5.25,height=2.25)  #  Figure 19.6
par(mfrow=c(1,3))
plot(nc,hill,type="b",main="(a)",ylab="Hill estimator")
plot(nc,hill,xlim=c(25,120),ylim=c(2,2.4),type="b",main="(b)",ylab=
  "Hill estimator")
plot(nc,hill,xlim=c(60,100),ylim=c(2,2.4),type="b",main="(c)",ylab=
  "Hill estimator")
hill[8:11]
graphics.off()



