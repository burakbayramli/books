# coding: utf-8

# Principal Component Analysis as an Example of Factor Model

import numpy as np
import pandas as pd
from numpy.linalg import eig
from statsmodels.api import OLS

lookback=252 # training period for factor exposure
numFactors=5
topN=50 # for trading strategy, long stocks with topN exepcted 1-day returns

df=pd.read_table('IJR_20080114.txt')
df['Date']=df['Date'].astype('int')
df.set_index('Date', inplace=True)
df.sort_index(inplace=True)
df.fillna(method='ffill', inplace=True)

dailyret=df.pct_change() # note the rows of dailyret are the observations at different time periods
positionsTable=np.zeros(df.shape)

for t in np.arange(lookback,df.shape[0]): 
    R=dailyret.iloc[t-lookback+1:t+1,].T # here the columns of R are the different observations.
    hasData=np.where(R.notna().all(axis=1))[0]
    R.dropna(inplace=True) # avoid any stocks with missing returns
    avgR=R.mean(axis=1)
    R=R.values-avgR.values.reshape((R.shape[0], 1)) # subtract mean from returns
    covR=pd.DataFrame(R.T).cov() # compute covariance matrix, with observations in rows.
    B, X=eig(covR) # X is the factor exposures matrix, B the variances of factor returns
    X=X[:, 0:numFactors] # Retain only numFactors
    model=OLS(R[:, -1], X)
    results=model.fit()
    b=results.params    # b are the factor returns for time period t-1 to t.
    Rexp=avgR+np.dot(X,b) # Rexp is the expected return for next period assuming factor returns remain constant.
    idxSort=Rexp.argsort()  
    
    positionsTable[t, hasData[idxSort.values[np.arange(0, topN)]]]=-1
    positionsTable[t, hasData[idxSort.values[np.arange(-topN,0)]]]=1
    

capital=np.nansum(np.array(abs(positionsTable).shift()), axis=1)
positionsTable[capital==0,]=0
capital[capital==0]=1
ret=np.nansum(np.array(pd.DataFrame(positionsTable).shift())*np.array(dailyret), axis=1)/capital
avgret=np.nanmean(ret)*252
