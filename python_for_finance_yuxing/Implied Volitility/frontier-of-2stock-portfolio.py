import numpy as np
import scipy as sp
import pandas as pd
from datetime import datetime as dt
from scipy.optimize import minimize
from scipy import sqrt
import matplotlib.pyplot as pl
# Step 1: input area
mean_0=(0.15,0.25)
std_0 = (0.10,0.20)
corr_=0.2
n=1000

n_stock=len(mean_0)
sp.random.seed(12345)
x1=sp.random.normal(loc=mean_0[0],scale=std_0[0],size=n)
x2=sp.random.normal(loc=mean_0[1],scale=std_0[1],size=n)
if (any(x1)<=-1.0 or any(x2)<=-1.0):
    print ('Error: return is <=-100%')
index_=pd.date_range(start=dt(2001,1,1),periods=n,freq='d')
y1=pd.DataFrame(x1,index=index_)
y2=pd.DataFrame(corr_*x1+sqrt(1-corr_**2)*x2,index=index_)
R0=pd.merge(y1,y2,left_index=True,right_index=True)
R=np.array(R0)
def objFunction(W, R, target_ret):
    stock_mean=np.mean(R,axis=0)
    port_mean=np.dot(W,stock_mean)
    cov=np.cov(R.T)
    port_var=np.dot(np.dot(W,cov),W.T)
    penalty = 2000*abs(port_mean-target_ret)
    return np.sqrt(port_var) +penalty
out_mean,out_std,out_weight=[],[],[]
stockMean=np.mean(R,axis=0)
##for r in np.linspace(np.min(stockMean), np.max(stockMean), num=100):
##    W = np.ones([n_stock])/n_stock
##    b_ = [(0,1) for i in range(n_stock)]
##    c_ = ({'type':'eq','fun': lambda W: sum(W)-1. })
##    result=minimize(objFunction,W,(R,r),method='SLSQP',constraints=c_,
##                    bounds=b_)
##    if not result.success:
##        raise BaseException(result.message)
##        out_mean.append(round(r,4))
##        std_=round(np.std(np.sum(R*result.x,axis=1)),6)
##        out_std.append(std_)
##        out_weight.append(result.x)
##    pl.title('Simulation for an efficient frontier from given 2 stocks')
##    pl.xlabel('Standard Deviation of the 2-stock portfolio')
##    pl.figtext(0.2,0.80,' mean = '+str(stockMean))
##    pl.figtext(0.2,0.75,'std ='+str(std_0))
##    pl.figtext(0.2,0.70,' correlation ='+str(corr_))
##    pl.plot(np.array(std_0),np.array(stockMean),'o',markersize=8)
##    pl.plot(out_std,out_mean,'--',linewidth=3)
pl.show()
        



