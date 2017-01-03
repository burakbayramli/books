
data(SP500,package="Ecdat")
SPreturn = SP500$r500

library(evir)  # for emplot
data(Garch,package="Ecdat")
attach(Garch)

data(Capm,package="Ecdat")
diffrf=diff(Capm$rf)

diffdm = diff(dm)  #  Deutsch mark
diffbp = diff(bp)  #  British pound
diffcd = diff(cd)  #  Canadian dollar
diffdy = diff(dy)  #  Japanese yen

n = length(SPreturn)
n2 = length(diffdm)
n3 = length(diffrf)
Reldiffrf = diffrf/(Capm$rf[2:(n3+1)])

year_SP = 1981 + (1:n)* (1991.25-1981)/n
year_dm = 1980 + (1:n2)* (1987.5-1980)/n2
year_rf = 1960 + (1:n3) * (2003 - 1960)/n3

postscript("risk_free_w_w0_log.ps",width=6,height=6) # Fig 4.27
par(mfrow=c(2,2))
plot(Capm$rf[-1],diff(Capm$rf),xlab="lagged rate",ylab="change in rate",
  main="(a)")
plot(year_rf,diff(Capm$rf),xlab="year",ylab="change in rate",
  main="(b)")
plot(Capm$rf[-1],diff(log(Capm$rf)),xlab="lagged rate",ylab="change in log rate",
  main="(c)")
plot(diff(log(Capm$rf)),xlab="year",ylab="change in log rate",
  main="(d)")
graphics.off()