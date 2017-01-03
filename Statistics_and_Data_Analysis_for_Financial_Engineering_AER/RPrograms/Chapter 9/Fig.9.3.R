#  Figure 9.3

data(Mishkin,package="Ecdat")

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 

year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)

#####   ACF plots 
postscript("inflation_acf.ps",height=3.5,width=6)     # Fig 9.3
par(mfrow=c(1,2))
acf(x,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Inflation rate")
acf(diff(x),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,
main="Change in inflation rate")
graphics.off()

Box.test(diff(x),lag=10)
