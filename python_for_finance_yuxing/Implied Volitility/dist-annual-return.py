from matplotlib.finance import quotes_historical_yahoo
import matplotlib.pyplot as plt
import numpy as np
import scipy as sp
# Step 1: input area
ticker='MSFT'   #input value    1
begdate=(1926,1,1)  # input value 2
enddate=(2013,12,31)    #input value 3
n_simulation=5000# input value 4
# step 2: retrieve price data and estimate log returns
x = quote_historical_yahoo(ticker,begdate,enddate,asobject=True,adjusted=True)
logret = log (x.aclose[1:]/x.close[:-1])
#step 3: estimate annual returns
date=[]
d0=x.date
for i in range(0,size(logret)):
    date.append(d0[i].strftime("%Y"))
y=pd.DataFrame(logret,date,colomns=['logret'],dtype=float64)
ret_annual=exp(y.groupby(y.index).sum())-1
ret_annual.columns=['ret_annual']
n_obs=len(ret_annual)
#Step 4: estimate distribution with replacemtn
sp.random.seed(123577)
final=zeros(n_obs,dtype=float)
for i in range(0,n_obs):
        x=sp.random.uniform(low=0,high=n_obs,size=n_obs)
        y=[]
    for j in range(n_obs):
y.append(int(x[j]))
z=np.array(ret_annual)[y]
final[i]=mean(z)
# step 5: graph
plt.title('Mean return distribution: number of simulations ='+str(n_
simulation))
plt.xlabel('Mean return')
plt.ylabel('Frequency')
mean_annual=round(np.mean(np.array(ret_annual)),4)
plt.figtext(0.63,0.8,'mean annual='+str(mean_annual))
plt.hist(final, 50, normed=True)
plt.show()
