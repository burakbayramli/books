# testCAPM.r
#
# author: Eric Zivot
# created: November 24, 2003
# updated: December 2, 2008
#
# comments
# 1. requires data in file berndt.csv

# read prices from csv file
berndt.df = read.csv(file="C:/FinBook/Data/berndt.csv", stringsAsFactors=F)
colnames(berndt.df)
#[1] "Date"   "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"    "DELTA"  "GENMIL"
# [9] "GERBER" "IBM"    "MARKET" "MOBIL"  "PANAM"  "PSNH"   "TANDY"  "TEXACO"
# [17] "WEYER"  "RKFREE"

# create zooreg object - regularly spaced zoo object

berndt.z = zooreg(berndt.df[,-1], start=c(1978, 1), end=c(1987,12),
                  frequency=12)
index(berndt.z) = as.yearmon(index(berndt.z))

start(berndt.z)
end(berndt.z)
nrow(berndt.z)

# create excess returns by subtracting off risk free rate
# note: coredata() function extracts data from zoo object
returns.mat = as.matrix(coredata(berndt.z))
excessReturns.mat = returns.mat - returns.mat[,"RKFREE"]
excessReturns.df = as.data.frame(excessReturns.mat)

# CAPM regression for CITCRP (citicorp) using 1st 5 years of data
capm.fit = lm(CITCRP~MARKET,data=excessReturns.df,subset=1:60)
summary(capm.fit)

# plot data and regression line
plot(excessReturns.df$MARKET,excessReturns.df$CITCRP,
main="CAPM regression for CITCRP",
ylab="Excess returns on CITCRP",
xlab="Excess returns on MARKET")
abline(capm.fit)					# plot regression line
abline(h=0,v=0)					# plot horizontal and vertical lines at 0

# CAPM regression for IBM using 1st 5 years of data
capm.fit = lm(IBM~MARKET,data=excessReturns.df,subset=1:60)
summary(capm.fit)

# plot data and regression line
plot(excessReturns.df$MARKET,excessReturns.df$IBM,
main="CAPM regression for IBM",
ylab="Excess returns on IBM",
xlab="Excess returns on MARKET")
abline(capm.fit)					# plot regression line
abline(h=0,v=0)					# plot horizontal and vertical lines at 0


# estimate CAPM and test alpha=0 for all assets using 1st 5 years of data
# trick use S-PLUS function apply to do this all at once

capm.tstats = function(r,market) {
	capm.fit = lm(r~market)					# fit capm regression
	capm.summary = summary(capm.fit)		# extract summary info
	t.stat = coef(capm.summary)[1,3]		# t-stat on intercept
	t.stat
}
# test function
tmp = capm.tstats(excessReturns.mat[1:60,1],
excessReturns.mat[1:60,"MARKET"])
tmp

# check out apply function
?apply

colnames(excessReturns.mat[,-c(10,17)])
tstats = apply(excessReturns.mat[1:60,-c(10,17)],2,
               FUN=capm.tstats,
               market=excessReturns.mat[1:60,"MARKET"])
tstats

# test H0: alpha = 0 using 5% test
abs(tstats) > 2
any(abs(tstats) > 2)

#
# plot average return against beta
#

# compute average returns over 1st 5 years
mu.hat = colMeans(excessReturns.mat[1:60,-c(10,17)])
mu.hat

# compute beta over 1st 5 years
capm.betas = function(r,market) {
	capm.fit = lm(r~market)					# fit capm regression
	capm.beta = coef(capm.fit)[2]				# extract coefficients
	capm.beta
}

betas = apply(excessReturns.mat[1:60,-c(10,17)],2,
              FUN=capm.betas,
              market=excessReturns.mat[1:60,"MARKET"])
betas

# plot average returns against betas
plot(betas,mu.hat,main="Ave return vs. beta")

# estimate regression of ave return on beta
sml.fit = lm(mu.hat~betas)
sml.fit
summary(sml.fit)

# intercept should be zero and slope should be excess return on market
mean(excessReturns.mat[1:60,"MARKET"])

plot(betas,mu.hat,main="TRUE and Estimated SML")
abline(sml.fit)
abline(a=0,b=mean(excessReturns.mat[1:60,"MARKET"]),lty=1, col="orange", lwd=2)
legend(0.2, 0.04, legend=c("Estimated SML","TRUE SML"),
       lty=c(1,1), col=c("black","orange"))


# compute average returns over 2nd 5 years
mu.hat2 = colMeans(excessReturns.mat[61:120,-c(10,17)])
mu.hat2

betas2 = apply(excessReturns.mat[61:120,-c(10,17)],2,
               FUN=capm.betas,
               market=excessReturns.mat[61:120,"MARKET"])
betas2

# plot average returns against betas
plot(betas2,mu.hat2,main="Ave return vs. beta")

# estimate regression of ave return on beta
sml.fit2 = lm(mu.hat2~betas2)
sml.fit2
summary(sml.fit2)

# intercept should be zero and slope should be excess return on market
mean(excessReturns.mat[61:120,"MARKET"])

plot(betas2,mu.hat2,main="TRUE and Estimated SML")
abline(sml.fit2)
abline(a=0,b=mean(excessReturns.mat[61:120,"MARKET"]),lwd=2, col="orange")
legend(0.2, -0.01, legend=c("Estimated SML","TRUE SML"),
       lty=c(1,1), col=c("black","orange"))


#
# prediction test II of CAPM
# estimate beta using 1st 5 years of data and
# compute average returns using 2nd 5 years of data
#

# estimate regression of 2nd period ave return on 
# 1st period beta
sml.fit12 = lm(mu.hat2~betas)
sml.fit12
summary(sml.fit12)

plot(betas,mu.hat2,main="TRUE and Estimated SML",
xlab="1st period betas",ylab="2nd period ave returns")
abline(sml.fit12)
abline(a=0,b=mean(excessReturns.mat[61:120,"MARKET"]),lty=2,col="orange")
legend(0.2, -0.01, legend=c("Estimated SML","TRUE SML"),
lty=c(1,2))

#
# estimate CAPM for portfolio
#
port = rowMeans(excessReturns.mat[,-c(10,17)])
new.df = data.frame(cbind(port,excessReturns.mat[,"MARKET"]))
colnames(new.df) = c("port","market")
port.fit = lm(port~market,data=new.df)
summary(port.fit)

plot(new.df$market,new.df$port,
     main="CAPM regression for Portfolio",
     ylab="Excess returns on portfolio",
     xlab="Excess returns on market")
abline(port.fit)					# plot regression line
abline(h=0,v=0)					# plot horizontal and vertical lines at 0

