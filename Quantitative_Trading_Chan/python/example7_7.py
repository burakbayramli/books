# Backtesting a Year-on-Year Seasonal Trending Strategy

import numpy as np
import pandas as pd

df=pd.read_table('SPX_20071123.txt')
df['Date']=df['Date'].round().astype('int')
df['Date']=pd.to_datetime(df['Date'], format='%Y%m%d')
df.set_index('Date', inplace=True)

eomPrice=df.resample('M').last()[:-1] # End of month prices.  Need to remove last date because it isn't really end of January.
monthlyRet=eomPrice.pct_change()

positions=np.zeros(monthlyRet.shape)

for m in range(13, monthlyRet.shape[0]):
    hasData=np.where(np.isfinite(monthlyRet.iloc[m-12, :]))[0]
    sortidx=np.argsort(monthlyRet.iloc[m-12, hasData])  
    badData=np.where(np.logical_not(np.isfinite(monthlyRet.iloc[m-1, hasData[sortidx]])))[0]
    sortidx.drop(badData, inplace=True)
    topN=np.floor(len(sortidx)/10).astype('int')
    positions[m-1, hasData[sortidx.values[np.arange(0, topN)]]]=-1
    positions[m-1, hasData[sortidx.values[np.arange(-topN,0)]]]=1

capital=np.nansum(np.array(pd.DataFrame(abs(positions)).shift()), axis=1)
positions[capital==0,]=0
capital[capital==0]=1
ret=np.nansum(np.array(pd.DataFrame(positions).shift())*np.array(monthlyRet), axis=1)/capital
avgret=np.nanmean(ret)*252
sharpe=np.sqrt(12)*np.nanmean(ret)/np.nanstd(ret)

print('Avg ann return=%f Sharpe ratio=%f' % (avgret, sharpe))