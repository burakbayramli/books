#  Example 17.5 and Figure 17.6

library("fEcofin")
CPI.dat[774:900,]
IP.dat[702:828,]
berndt =as.matrix(berndtInvest[,-1])   #  1978-01-01 to 1987-12-01
CPI = as.data.frame(diff(log(CPI2))) 
CPI2 = as.matrix(CPI.dat$CPI[775:900]) #  1977-07-30  to 1987-12-31
IP = as.data.frame(diff(log(IP2)))
IP2 = as.matrix(IP.dat$IP)[703:828,]   #  1977-07-28 to 1987-12-28 
arFit = ar(cbind(CPI,IP))
res = arFit$resid[6:125,]
lmfit = lm(berndt[,2:10]~res[,1]+res[,2])
slmfit = summary(lmfit)
rsq=rep(0,9)
for (i in 1:9){rsq[i]= slmfit[[i]][[8]]}
beta_CPI = lmfit$coef[2,]
beta_IP = lmfit$coef[3,]

postscript("macrofactors.ps",width=6,height=7)  #  Figure 17.6
par(mfrow=c(1,3))
barplot(rsq,horiz=T,names=names(beta_CPI),main="R squared")
barplot(beta_CPI,hori=T,main="beta CPI")
barplot(beta_IP,hori=T,main="beta IP")
graphics.off()


