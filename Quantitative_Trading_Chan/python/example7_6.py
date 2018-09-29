# Backtesting the January Effect

import numpy as np
import pandas as pd

onewaytcost=0.0005

df=pd.read_table('IJR_20080131.txt')
df['Date']=df['Date'].round().astype('int')
df['Date']=pd.to_datetime(df['Date'], format='%Y%m%d')
df.set_index('Date', inplace=True)

eoyPrice=df.resample('Y').last()[0:-1] # End of December prices. Need to remove last date because it isn't really end of year
annret=eoyPrice.pct_change().iloc[1:,:] # first row has NaN

eojPrice=df.resample('BA-JAN').last()[1:-1] # End of January prices. Need to remove first date to match the years in lastdayofDec. Need to remove last date because it isn't really end of January.
janret=(eojPrice.values-eoyPrice.values)/eoyPrice.values
janret=janret[1:,] # match number of rows in annret

for y in range(len(annret)):
    hasData=np.where(np.isfinite(annret.iloc[y, :]))[0]
    sortidx=np.argsort(annret.iloc[y, hasData])
    topN=np.round(len(hasData)/10)
    portRet=(np.nanmean(janret[y, hasData[sortidx.iloc[np.arange(0, topN)]]])-np.nanmean(janret[y, hasData[sortidx.iloc[np.arange(-topN+1, -1)]]]))/2-2*onewaytcost # portfolio returns

    print('Last holding date %s: Portfolio return=%f' % (eojPrice.index[y], portRet))
