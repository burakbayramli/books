"EWMAvol" <- function(rtn,theta=0.94){
# Compute the exponentially weighted moving average covariance matrix.
#
if(!is.matrix(rtn))rtn=as.matrix(rtn)
S=1e-6
if(theta < S)theta=0.9
if(theta > (1-S))theta = 0.98
# variance
k=dim(rtn)[2]
T=dim(rtn)[1]
V=NULL
for (i in 1:k){
x=rtn[,i]
Mean=mean(x^2)
e=(1-theta)*c(Mean,x[-T]^2)
v=filter(e,theta,"r",init=Mean)
V=cbind(V,v)
}
for (i in 1:(k-1)){
for (j in (i+1):k){
x=rtn[,i]*rtn[,j]
Mean=mean(x)
e=(1-theta)*c(Mean,x[-T])
v=filter(e,theta,"r",init=Mean)
V=cbind(V,v)
}
}

V
}

