#  Figure 10.3
#  The x-axis of this Figure is mislabeled.
#  The program corrects that error and adds a legend to the plot

data(Hstarts,package="Ecdat")
hst = Hstarts[,1]
t = seq(1960.25,2002,.25)
year = t
n = length(t)

fit2 = arima(hst,c(1,1,1),seasonal = list(order = c(0,1,1), period = 4))
pred = predict(fit2, n.ahead = 16, newxreg = NULL,
        se.fit = TRUE)
t1 = 130:168
t2 = 169:(169+15)
year = c(year,seq(2002,2010,by=.25))

postscript("Hstarts_predict.ps",width=6,height=5)  #  Figure 10.3
plot(year[t1],hst[t1],ylim=c(8.25,10.3),type="b",lty=2,xlim=c(1992,2006),
   xlab="year",ylab="log(starts)")

points(year[t2], as.matrix(pred$pred),type="b",pch="*",lty=3)
lines(year[t2], pred$pred - 2*pred$se)
lines(year[t2], pred$pred + 2*pred$se)
legend("topleft",c("data","predictions","lower CL","upper CL"),
   pch=c("o","*",NA,NA),lty=c(2,3,1,1))
graphics.off()


