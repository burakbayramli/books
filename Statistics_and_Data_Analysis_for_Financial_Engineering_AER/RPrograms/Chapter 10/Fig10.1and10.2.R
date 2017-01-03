#   Figures 10.1 and 10.2

data(Hstarts,package="Ecdat")
hst = Hstarts[,1]
x =hst
t = seq(1960.25,2002,.25)
year =t
n = length(t)
fit = arima(hst,c(1,1,1),seasonal = list(order = c(1,1,1), period = 4))
fit2 = arima(hst,c(1,1,1),seasonal = list(order = c(0,1,1), period = 4))

pred =predict(fit2, n.ahead = 16, newxreg = NULL,
        se.fit = TRUE)
t1 = 130:168
t2 = 169:(169+15)
x = as.matrix(x)

postscript("Hstart.ps",width=5.5,height=2.5)  #  Figure 10.1
par(mfrow=c(1,3))
plot(t,x,ylab="log(starts)",type="b",xlab="year",main="(a)")
acf(x,main="(b)",xlab="lag")
quart = rep(1,42) %x% (1:4)
boxplot(x~quart,xlab="quarter",ylab="log(starts)",main="(c)")
graphics.off()

postscript("Hstart_diff_plots.ps",width=6,height=6)   #  Fig 10.2
par(mfrow=c(3,2))
plot(year[2:n],diff(x),xlab="year",type="b",
   main="(a) nonseasonal differencing")
acf(diff(x),main="(b) nonseasonal differencing",xlab="lag")
plot(year[5:n],diff(x,4),type="b",xlab="year",
   main="(c) seasonal differencing")
acf(diff(x,4),main="(d) seasonal differencing",xlab="lag")
plot(year[6:n], diff(diff(x,1),4),type="b",xlab="year",
   main="(e) seasonal & nonseasonal differencing")
acf( diff(diff(x,1),4),main="(f) seasonal & nonseasonal differencing",
   xlab="lag")
graphics.off()


