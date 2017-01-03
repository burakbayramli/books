"ma" <- function(pri,n,plot=TRUE){
# pri: price series of an asset (univariate)
# n: window size
#
nob=length(pri)
ma1=pri
range=max(pri)-min(pri)
if(nob > n){
psum=sum(pri[1:(n-1)])
ma1[1:n]=psum/(n-1)
for (i in n:nob){
psum=psum+pri[i]
ma1[i]=psum/n
psum=psum-pri[i-n+1]
}
}
if(plot){
par(mfcol=c(1,1))
plot(pri,type='l',xlab="time index")
lines(ma1,lty=2)
loc=max(pri)-range/3
legend(n/2,loc,c(paste("n = ",c(n))),lty=2)
title(main='Moving average plot')
}
ma <- list(ma=ma1)
}
