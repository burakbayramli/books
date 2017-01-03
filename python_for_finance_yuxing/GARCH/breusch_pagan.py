import numpy as np
import statsmodels.api as sm
import scipy as sp

def breusch_pagan_test(y,x):
results=sm.OLS(y,x).fit()
resid=results.resid
Chapter 12
[ 357 ]
n=len(resid)
sigma2 = sum(resid**2)/n
f = resid**2/sigma2 - 1
results2=sm.OLS(f,x).fit()
fv=results2.fittedvalues
bp=0.5 * sum(fv**2)
df=results2.df_model
p_value=1-sp.stats.chi.cdf(bp,df)
return round(bp,6), df, round(p_value,7)
sp.random.seed(12345)
n=100
x=[]
error1=sp.random.normal(0,1,n)
error2=sp.random.normal(0,2,n)
for i in range(n):
if i%2==1:
x.append(1)
else:
x.append(-1)
y1=x+np.array(x)+error1
y2=zeros(n)
for i in range(n):
if i%2==1:
y2[i]=x[i]+error1[i]
else:
y2[i]=x[i]+error2[i]
print ('y1 vs. x (we expect to accept the null hypothesis)')
bp=breusch_pagan_test(y1,x)
print('BP value, df, p-value')
print 'bp =', bp
bp=breusch_pagan_test(y2,x)
Volatility Measures and GARCH
[ 358 ]
print ('y2 vs. x (we expect to rject the null hypothesis)')
print('BP value, df, p-value')
print('bp =', bp)
