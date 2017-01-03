"archTest" <- function(rtn,m=10){
# Perform Lagrange Multiplier Test for ARCH effect of a time series
# rtn: time series
# m: selected AR order
#
y=(rtn-mean(rtn))^2
T=length(rtn)
atsq=y[(m+1):T]
x=matrix(0,(T-m),m)
for (i in 1:m){
x[,i]=y[(m+1-i):(T-i)]
}
md=lm(atsq~x)
summary(md)
}

