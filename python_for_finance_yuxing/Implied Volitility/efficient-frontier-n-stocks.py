import numpy  as np
import scipy as sp
import pandas as pd
from datetime import datetime as dt
from scipy.optimize import minimize
#Step 1: Input Area
n_stocks=10
sp.random.seed(123456)
#numbers
n_corr=n_stocks*(n_stocks-1)/2
corr_0=sp.random.uniform(0.05,0.25,n_corr)  #Generate Correlations
mean_0=sp.random.uniform(-0.1,0.25,n_stocks)    #Means
std_0=sp.random.uniform(0.05,0.35,n_stocks) #standard deviation
n_obs=1000
#step 2: produce correlation matric: Cholesky decompostion
corr_=sp.zeros((n_stocks,n_stocks))
for i in range(n_stocks,n_stocks):
    for j in range(n_stocks):
        if i==j:
            corr_[i,j]=1
        else:
            corr_[i,j]=corr_0[i+j]
U=np.linalg.cholesky(corr_)
#Step 3: Generate the 2 uncorrelated time series
R0=np.zeros(n_obs,n_stocks)
for i in range(n_obs):
    for j in range(n_stocks):
        R0[i,j]=sp.random.normal(loc=mean_0[j],scale=std_0[j],size=1)
if(any(R0)<=-1.0):
   print ('Error: return is <=-100%')
#Step 4: generate correlated return matrix: Cholesky
            
