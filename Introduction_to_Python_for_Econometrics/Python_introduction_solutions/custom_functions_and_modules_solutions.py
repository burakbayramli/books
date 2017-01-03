"""Solutions for 'Custom Functions and Modules' chapter.

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('standard_datatypes_solutions.py')

and then call

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
from numpy import mat, asarray, unique, hstack, logical_and, corrcoef, logical_not, zeros, sort, r_, eye
from numpy.linalg import inv
import numpy as np

# <demo> --- stop ---
# Exercise 1
def ascategory(x):
    """
    """
    T = x.shape[0]
    ux = unique(x)
    K = ux.shape[0]
    categories = np.zeros((T,K))
    for i in xrange(K):
        loc = np.squeeze(x == ux[i])
        categories[loc,i] = 1.0
    
    return categories

# <demo> --- stop ---
# Exercise 2
def gls(X,y,Omega):
    """
    """
    T,K = X.shape
    OmegaInv = mat(inv(Omega))
    Xm = mat(X)
    XpX = Xm.T*OmegaInv*Xm
    Xpy = Xm.T*OmegaInv*mat(y)
    betaGLS = asarray(inv(XpX)*Xpy)
    return betaGLS

# <demo> --- stop ---
# Exercise 3
def partial_corr(x, y=None, quantile=0.5, tail='Lower'):
    """
    """
    if y is not None:
        X = x.view()
        Y = y.view()
        T = X.shape[0]
        X.shape = T,1
        Y.shape = T,1
        z = hstack((X,Y))
    else:
        z = x
    
    T,K = z.shape
    corr = eye(K)
    count = zeros((K,K))
    ind = zeros((T,K),dtype=np.bool)
    for i in xrange(K):
        temp = sort(z[:,i].ravel())
        cutoff = int(round(quantile*T))
        threshold = temp[cutoff]
        ind[:,i] = z[:,i]<threshold
        if tail=='Upper':
            ind[:,i] = logical_not(ind[:,i])
            
    for i in xrange(K):
        for j in xrange(i+1,K):
            pl = logical_and(ind[:,i],ind[:,j])
            count[i,j] = sum(pl)
            count[j,i] = count[i,j] 
            if sum(pl)>1:
                w = z[pl,:]
                w = w[:,r_[i,j]]
                corr[i,j] = corrcoef(w.T)[0,1]
                corr[j,i] = corr[i,j]
            else:
                corr[i,j] = np.nan
                corr[j,i] = np.nan
    
    return corr, count
# <demo> --- stop ---
