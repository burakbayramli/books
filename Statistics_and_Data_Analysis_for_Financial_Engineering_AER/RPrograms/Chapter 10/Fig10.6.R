#  Figure 10.6


library("fEcofin")

CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
IP = as.matrix(IP.dat$IP)[697:828,]  # 1977-01-31 to 1987-12-31
IP_diff1 = as.matrix(diff(log(IP)))
CPI_IP = as.data.frame(na.omit(cbind(CPI_diff1,IP_diff1)))
names(CPI_IP) = c("Delta CPI","Delta IP")

postscript("CPI_IP_ccf.ps",height=6,width=8)  #  Figure 10.6
ccf(CPI_diff1[,],IP_diff1[,],lwd=3,  
   main=expression(paste("corr{",Delta,"CPI(t),",Delta,"IP(t-lag) }" )))
abline(v=0,lty=5)
graphics.off()