#  Figure 9.2

data(AirPassengers)
x = as.matrix(AirPassengers)
n = length(x)
year = 1949 + (0:(n-1))/12



postscript("airpass.ps")               #    Figure 9.2
plot(year,x,type="b",ylab="passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,
   lwd=2)
graphics.off()



