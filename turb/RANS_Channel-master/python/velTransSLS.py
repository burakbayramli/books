# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 14:03:22 2019

@author: gjoterorodrigu
"""

def velTransSLS(u,r,mu,ReTau,mesh):

    import numpy as np
    from velTransVD import velTransVD

    n = mesh.nPoints

    # Calculating correction factor of semi-local velocoty transformation
    ReTauStar = ReTau*np.sqrt(r/r[0])/(mu/mu[0])
    dRTSdy    = mesh.ddy@ReTauStar
    fact      = 1 + mesh.y/ReTauStar*dRTSdy

    ystar = mesh.y*ReTauStar
    ypl, uvd = velTransVD(u,r,ReTau,mesh)
    
    ustar = np.zeros(n)
    for i in range(1,n):
        ustar[i] = ustar[i-1] + 0.5*(fact[i]+fact[i-1])*(uvd[i]-uvd[i-1])

    return ystar,ustar
