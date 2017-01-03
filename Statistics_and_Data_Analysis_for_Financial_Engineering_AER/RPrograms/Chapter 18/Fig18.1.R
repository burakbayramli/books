data(Mishkin,package="Ecdat")
infl= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 
data(SP500,package="Ecdat")
SPreturn = SP500$r500

library(evir)  # for emplot
data(Garch,package="Ecdat")
attach(Garch)

data(Capm,package="Ecdat")
difflogrf=diff(log(Capm$rf))

diffdm = diff(dm)  #  Deutsch mark
diffbp = diff(bp)  #  British pound
diffcd = diff(cd)  #  Canadian dollar
diffdy = diff(dy)  #  Japanese yen

n  = length(SPreturn)
n2 = length(diffdm)
n3 = length(difflogrf)
n4 = length(infl)

year_SP = 1981 + (1:n)* (1991.25-1981)/n
year_dm = 1980 + (1:n2)* (1987.5-1980)/n2
year_rf = 1960 + (1:n3) * (2003 - 1960)/n3
year_infl = 1950+1/12 + (1:n4) * (1991-1950-1/12)/n4

postscript("garch_examples.ps")
lwdfact=6
par(mfrow=c(2,2))
plot(year_SP,abs(SPreturn),main="S&P 500 daily return",xlab="year",type="l",
   cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="|log return|",ylim=c(0,.08))
mod = loess( abs(SPreturn)~year_SP,span=.25)
lines(year_SP,predict(mod),lwd=lwdfact,col=gray(.7) )

plot(year_dm,abs(diffbp),xlab="year",ylab="|change in rate|",main=
   "BP/dollar exchange rate",type="l",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
mod = loess( abs(diffbp)~year_dm,span=.25)
lines(year_dm,predict(mod),lwd=lwdfact,col=gray(.7) )

plot(year_rf,abs(difflogrf),main="Risk-free interest rate",xlab="year",
    ylab="|change in log(rate)|",type="l",cex.axis=1.5,cex.main=1.5,cex.lab=1.5)
mod = loess( abs(difflogrf)~year_rf,span=.25)
lines(year_rf,predict(mod),lwd=lwdfact,col=gray(.7) )

plot(year_infl,abs(infl-mean(infl)),ylab="|rate - mean(rate)|",xlab="year",
   type="l",cex.axis=1.5,cex.main=1.5,cex.lab=1.5,main="Inflation rate")

mod = loess( abs(infl-mean(infl))~year_infl,span=.3)
lines(year_infl,predict(mod),lwd=lwdfact,col=gray(.7) )

graphics.off()


