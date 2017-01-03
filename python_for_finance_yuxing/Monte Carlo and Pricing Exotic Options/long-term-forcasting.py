from matplotlib.finance import quotes_historical_yahoo
import pandas as np
import pandas as pd
ticker='IBM'
begdate=(1926,1,1)
enddate=(2013,12,31)
n_forecast=15.

def geomean_ret(returns):
    product = 1
    for ret in returns:
        product *= (1+ret)
    return product ** (1.0/len(returns))-1

x=quotes_historical_yahoo(ticker,begdate,enddate,asobject=True, adjusted=True)
logret=log(x.aclose[1:]/x.aclose[:-1])
date=[]
d0=x.date
for i in range(0,size(logret)):
    date.append(d0[i].strftime("%Y"))

