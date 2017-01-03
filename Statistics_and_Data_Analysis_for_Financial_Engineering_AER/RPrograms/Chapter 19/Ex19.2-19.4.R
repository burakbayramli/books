#  Ex 19.2, 19.3, and 19.4

data(SP500,package="Ecdat")
library("fGarch")
# daily observations from 1981–01 to 1991–04 
# number of observations : 2783 
# daily return S&P500 (change in log index) 
n = 2783
SPreturn = SP500$r500[(n-999):n]
year = 1981 + (1:n)* (1991.25-1981)/n
year = year[(n-999):n]

n = length(SPreturn)
grid = (1:n)/(n+1)

postscript("SPreturns_tplot.ps",width=6,height=5)  # Figure 19.2
par(mfrow=c(1,1))
qqplot(SPreturn, qt(grid,df=2.9837),main="t-probability plot, 
   df=2.9837",xlab="data",ylab="t-quantiles")
abline(lm(qt(c(.25,.75),df=2.9837)~quantile(SPreturn,c(.25,.75))))
graphics.off()

################  Example 19.2 nonparametric estimation ###############
alpha = 0.05
q = as.numeric(quantile(SPreturn,alpha))
VaR_nonp = -20000*q
IEVaR = (SPreturn < q)
ES_nonp = -20000 * sum(SPreturn*IEVaR) / sum(IEVaR)
VaR_nonp
ES_nonp

#############  Example 19.3 parametric estimation ############
fitt = fitdistr(SPreturn,"t")
param = as.numeric(fitt$estimate)
mean = param[1]
df = param[3]
sd = param[2]*sqrt( (df)/(df-2) )
lambda = param[2]  #  scale parameter
qalpha = qt(alpha,df=df)
VaR_par = -20000*(mean + lambda*qalpha)

es1 = dt(qalpha,df=df)/(alpha)
es2 = (df + qalpha^2) / (df - 1)
es3 = -mean+lambda*es1*es2
ES_par = 20000*es3
VaR_par
ES_par


############ Example 19.4 bootstrapping ##########
B = 5000
VaRs=matrix(0,nrow=B,ncol=4)
set.seed(38751)
ptm1 <- proc.time()
for (i in (1:B))
{
returns_b = sample(SPreturn,1000,replace=TRUE)
q_b = as.numeric(quantile(returns_b,.05))
VaR_nonp_b = -20000*q_b
IEVaR_b = (returns_b < q_b)
ES_nonp_b = -20000 * sum(returns_b*IEVaR_b) / sum(IEVaR_b)

fitt_b = fitdistr(returns_b,"t")
param_b = as.numeric(fitt_b$estimate)
mean_b = param_b[1]
df_b = param_b[3]
sd_b = param_b[2]*sqrt( (df_b)/(df_b-2) )
lambda_b = param_b[2]
qalpha_b = qt(.05,df=df_b)
VaR_par_b = -20000*(mean_b + lambda_b*qalpha_b)
es1_b = dt(qalpha_b,df=df_b)/(alpha)
es2_b = (df_b + qalpha_b^2) / (df_b - 1)
es3_b = -mean_b+lambda_b*es1_b*es2_b
ES_par_b = 20000*es3_b
VaRs[i,]=c(VaR_nonp_b,VaR_par_b,ES_nonp_b,ES_par_b)
}
ptm2 = proc.time()
(ptm2  -  ptm1)/60
colnames(VaRs) = c("VaR_nonp","VaR_par","ES_nonp","ES_par")
apply(VaRs,2,mean)

print(quantile(VaRs[,1],c(.05,.95)),digits=3)
print(quantile(VaRs[,2],c(.05,.95)),digits=3)
print(quantile(VaRs[,3],c(.05,.95)),digits=3)
print(quantile(VaRs[,4],c(.05,.95)),digits=3)

mu1 = mean(VaRs[1:50,1])
sd1 = sd(VaRs[1:50,1])
mu1-1.645*sd1
mu1+1.645*sd1







