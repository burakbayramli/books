#  Example 9.10 including Fig 9.15 - 9.16

library("fEcofin")
library("tseries")
options(digits=5)

CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
CPI_diff2 = as.matrix(diff(log(CPI),diff=2))
n = length(CPI)
year = as.vector(1977 + (1987-1977)*(1:n)/n)

IP2 = as.matrix(IP.dat$IP)[697:828,]  # 1977-01-31 to 1987-12-31
IP_diff1 = as.matrix(diff(log(IP2)))

CPI_IP = as.data.frame(na.omit(cbind(CPI_diff1,IP_diff1)))
names(CPI_IP) = c("Delta CPI","Delta IP")

adf.test(CPI)
adf.test(CPI_diff1)
adf.test(CPI_diff2)
PP.test(CPI_diff1)

postscript("CPI_differences.ps",height=7,width=8)      #  Fig 9.15
par(mfrow=c(2,2),cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
plot(year,log(CPI),xlab="year",ylab="log(CPI)",type="b",main="(a)")
plot(year[2:n],as.vector(CPI_diff1),xlab="year",
   ylab=expression(paste(Delta," log(CPI)")),type="b",main="(b)")
plot(year[3:n],CPI_diff2,xlab="year",
   ylab=expression(paste(Delta^2," log(CPI)")),type="b",main="(c)")
graphics.off()

postscript("CPI_differences_acf.ps",height=7,width=8)     #  Fig 9.16
par(mfrow=c(2,2),cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
acf(log(CPI),main="(a) log(CPI)")
acf(CPI_diff1,main=expression(paste("(b) ",Delta," log(CPI}")))
acf(CPI_diff2,main=expression(paste("(c) ",Delta^2," log(CPI}")))
library(forecast)
fit_auto = auto.arima(CPI_diff2,ic="bic")
fit_ma = arima(CPI_diff2,order=c(0,0,2))
acf(fit_ma$resid,main="(d) residuals, ARIMA(0,2,2)")
graphics.off()

Box.test(fit_ma$resid,lag=15)
adf.test(IP_diff1)
fit_arma_IP = arima(IP_diff1,order=c(1,0,1))
auto.arima(IP_diff1,ic="bic")

postscript("IP_differences.ps",height=5.5,width=6)  #  Figure 9.17
par(mfrow=c(2,2))
plot(log(IP2),main="(a) IP",type="b")
plot(diff(IP2),main=expression(paste("(b) ",Delta," IP")),type="b")
acf(diff(IP2),main=expression(paste("(c) ","ACF of ", Delta, " IP")))
acf(fit_arma_IP$resid,main="(d) ACF of ARMA(1,1) residuals")
graphics.off()

