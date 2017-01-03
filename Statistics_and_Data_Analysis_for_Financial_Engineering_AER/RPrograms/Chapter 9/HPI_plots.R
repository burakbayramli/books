library("fracdiff")

dat = read.csv("HPI_MonthlyIndex_to_1991NoHeader.csv",sep=",",header=F)
names(dat) = c("Month",	"EaNoCe_NSA",  "EaNoCe_SA","EaSoCe_NSA",
  "EaSoCe_SA","MidAt_NSA","MidAt_SA","MTN_NSA","MTN_SA")

dat[1:5,1:9]
n = dim(dat)[1]

year = 1991 + (2008+9/12-1991)*(1:n)/n


plot(year,dat$EaNoCe_SA,ylim=c(98,290),type="l",lty=1)
points(year,dat$EaSoCe_SA,pch=2,type="l",lty=2)
points(year,dat$MidAt_SA,pch=3,type="l",lty=3)
points(year,dat$MTN_SA,pch=4,type="l",lty=4)

plot(year[2:n],diff(dat$EaNoCe_SA),type="b")
acf(diff(dat$EaNoCe_SA),lag=50)

summary(fracdiff(dat$EaNoCe_SA,nar=0,nma=1))
