#  Example 10.4


library("fEcofin")


CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
IP = as.matrix(IP.dat$IP)[697:828,]  # 1977-01-31 to 1987-12-31
IP_diff1 = as.matrix(diff(log(IP)))
CPI_IP = as.data.frame(na.omit(cbind(CPI_diff1,IP_diff1)))
names(CPI_IP) = c("Delta CPI","Delta IP")

arFit1 = ar(CPI_IP,order.max=1)
arFit