#  Figures 9.18 and 9.19

data(Mishkin,package="Ecdat")

infl = as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 

year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)
fit=arima(infl,c(0,1,3))
pred.infl = predict(fit, n.ahead = 100, se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)
year = seq(1950 + 1/12,2001+61/12,1/12)

postscript("Inflation_predict.ps",width=7,height=6)   #  Figure 9.19
plot(year[t1],infl[t1],ylim=c(-10,18),type="b",xlim=c(1975,1999),
   xlab="year",ylab="Inflation rate",cex.axis=1.15,cex.lab=1.15)
points(year[t2], pred.infl$pred,type="p",pch="*")
lines(year[t2], pred.infl$pred - 2*pred.infl$se)
lines(year[t2], pred.infl$pred + 2*pred.infl$se)
legend(1975,-3,c("data","predictions","lower CL","upper CL"),cex=1.2,
   box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))
graphics.off()

fit_diff=arima(diff(infl),c(0,0,3))

pred.infl_diff = predict(fit_diff, n.ahead = 100, newxreg = NULL,
        se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)

postscript("Diff_Inflation_predict.ps",width=7,height=6)  #  Figure9.18
plot(year[t1],diff(infl)[t1],xlim=c(1975,1999),ylim=c(-9,15),type="b",
   xlab="year",ylab="Change in inflation rate",cex.axis=1.5,cex.lab=1.5)
points(year[t2], pred.infl_diff$pred,type="p",pch="*")
lines(year[t2], pred.infl_diff$pred - 2*pred.infl_diff$se)
lines(year[t2], pred.infl_diff$pred + 2*pred.infl_diff$se)
legend(1975,14,c("data","predictions","lower CL","upper CL"),cex=1.2,
   box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))

graphics.off()







