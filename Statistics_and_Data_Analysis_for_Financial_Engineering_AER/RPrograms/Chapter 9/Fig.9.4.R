#  Figure 9.4

data(bmw,package="evir")
bmw = as.vector(bmw)
n=length(bmw)

postscript("BMW_acf.ps",height=5,width=6)     #   Figure 9.4
acf(bmw,lag.max=20,cex.axis=1.15,cex.lab=1.15,main="BMW log returns",cex.main=1.2)
graphics.off()