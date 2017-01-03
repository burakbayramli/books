#  Ex 9.16 including Figures 9.20 - 9.22

data(Mishkin,package="Ecdat")

infl = as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)
fit_diff=arima(diff(infl),c(0,0,3))
pred.infl_diff =predict(fit_diff, n.ahead = 100, newxreg = NULL,
        se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)
resid = fit_diff$resid[488:490]
coeff = as.vector(fit_diff$coef[1:3])
mu = as.vector(fit_diff$coef[4])
niter = 50000
n.ahead =30
futureobs = matrix(0,nrow=niter,ncol=n.ahead)
future_int = futureobs

set.seed(1234576)
for (i in 1:niter)
{
errors = sample(fit_diff$resid, n.ahead, replace = TRUE)
#  errors = rnorm(30,mean=0,sd = sqrt(fit_diff$sigma2))
errors = c(resid,errors)
for (j in 1:n.ahead)
{
futureobs[i,j] = mu + errors[j+3] + errors[j+2]*coeff[1]+ 
     errors[j+1]*coeff[2] + errors[j]*coeff[3]
if (j > 1)
{
future_int[i,j] = future_int[i,j-1] + futureobs[i,j]
}
if (j==1){future_int[i,j] = futureobs[i,j]
}
}
}
future_mean = apply(futureobs,2,mean)
ul = 0*(1:n.ahead)
ll =ul
for (k in 1:n.ahead)
{
ul[k] = quantile(futureobs[,k],.975)
ll[k] = quantile(futureobs[,k],.025)
}

postscript("inflation_forecasts_sim.ps")   #  Figure 9.21
plot(1:n.ahead,ul,ylim=c(-10,10),type="b",lwd=2,xlab="month ahead",
    ylab="rate",cex.axis=1.5,cex.lab=1.5)
lines(ll,type="b",lwd=2)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] - 
     1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] + 
     1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, future_mean,lwd=2,lty=2)
graphics.off()

postscript("inflation_forecasts_sim_random.ps"))   #  Figure 9.20
plot(1:n.ahead,futureobs[1,],ylim=c(-12,12),
  type="b",xlab="month ahead",ylab="rate",cex.axis=1.5,
  cex.lab=1.5,lwd=2)
for (i in 2:5)
{
lines(1:n.ahead,futureobs[i,],type="b",lty=i,lwd=2)
}
graphics.off()


ul_int = 0*(1:n.ahead)
ll_int =ul_int
for (k in 1:n.ahead)
{
ul_int[k] = quantile(future_int[,k],.975)
ll_int[k] = quantile(future_int[,k],.025)
}
future_mean_int = apply(future_int,2,mean)

postscript("inflation_forecasts_sim_integrated.ps")  #   Figure 9.22
plot(1:n.ahead,ul_int,ylim=c(-5,15),type="b",lwd=2,xlab="month ahead",
   ylab="rate",cex.axis=1.5,cex.lab=1.5)
lines(ll_int,type="b",lwd=2)
lines(future_mean_int)
graphics.off()
