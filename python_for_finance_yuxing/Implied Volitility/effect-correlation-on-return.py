import numpy as np
import scipy as sp
import pandas as pd
from datetime import datetime as dt
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from math import sqrt
mean_0=(0.15,0.25)
std_0=(0.10,0.20)
n=1000
corr_=(0.1,0.5,0.8)
n_stock=len(mean_0)
x11=sp.random.normal(loc=0,scale=1,size=n)
x12=sp.random.normal(loc=0,scale=1,size=n)
n_corr=len(corr_)
style_=['-.','--','-']

for j in range(n_corr):
    corr2=corr_[j]
    index_=pd.date_range(start=dt(2001,1,1),periods=n,freq='d')
    x21=pd.DataFrame(x11,index=index_)
    x22=pd.DataFrame(corr2*x11+sqrt(1-corr2**2)*x12,index=index_)
    y1=mean_0[0]+x21*std_0[0]
    y2=mean_0[1]+x22*std_0[1]
    R0=pd.merge(y1,y2,left_index=True,right_index=True)
    R=np.array(R0)
def objFunction(W,R,target_ret):
    stock_mean=np.mean(R,axis=0)
    port_mean=np.dot(W,stock_mean)
    cov=np.cov(R,T)
    port_var=np.dot(np.dor(W,cov),W.T)
    penalty = 2000*abs(port_mean-target_ret)
    return np.sqrt(port_var) + penalty
    print('Stock mean=',stockMean)
out_mean,out_std,out_weight=[],[],[]
stockMean=np.mean(R,axis=0)
print('hahastyle[j]',stockMean)
for r in np.linspace(np.min(stockMean), np.max(stockMean), num=100):
    W = np.ones([n_stock])/n_stock
    b_ = [(0,1) for i in range(n_stock)]
    c_ = ({'type','eq','fun',lambda W: sum(W)-1. })
    result=minimize(objFunction,W,(R,r),method='SLSQP',constraints=c_, bounds=b_)
    if not result.success:
        raise BaseException(result.message)
        out_mean.append(round(r,4))
        std_=round(np.std(np.sum(R*result.x,axis=1)),6)
        out_std.append(std_)
        outweight.append(result.x)
    plt.plot(out_std,out_mean,style_[j],label='corr='+str(corr2),linewidth=3)
                   
                   

