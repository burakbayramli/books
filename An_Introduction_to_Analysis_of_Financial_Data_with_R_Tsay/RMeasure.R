"RMeasure" <- function(mu,sigma,cond.dist="norm",df=0){
# calculate VaR and ES for a specified conditional distribution
# p = 0.05, 0.01, 0.001
#
# cond.dist = "norm", "t", "std"
prob=c(0.95,0.99,0.999)
if(cond.dist=="norm"){
q1=qnorm(prob)
d1=dnorm(q1)
VaR=mu+q1*sigma
ES=mu+d1/(1-prob)*sigma
tt=cbind(prob,VaR,ES)
}
#
if(cond.dist=="std"){
library(fGarch)
if(df < 2.001)df=2.01
q1=qstd(prob,nu=df)
d1=dstd(q1,nu=df)
VaR=mu+q1*sigma*sqrt(df/(df-2))
ES=mu+sigma*sqrt(df/(df-2))*(d1/(1-prob))*(((df-2)+q1^2)/(df-1))
tt=cbind(prob,VaR,ES)
}
#
if(cond.dist=="t"){
if(df < 2.01)df=2.01
q1=qt(prob,df)
d1=dt(q1,df)
VaR=mu+q1*sigma
ES=mu+sigma*(d1/(1-prob))*((df+q1^2)/(df-1))
tt=cbind(prob,VaR,ES)
}
cat("\n Risk Measures for selected probabilities: \n")
print(tt)

RMeasure <- list(results=tt)
}
