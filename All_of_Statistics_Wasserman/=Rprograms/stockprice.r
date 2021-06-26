time  <- seq(0,10,length=1000)
price <- cos(time) + rnorm(1000,0,1) + 10
postscript("stockprice.eps",horizontal=F,onefile=F,print.it=F)
plot(time,price,type="l",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
dev.off()

