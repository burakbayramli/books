# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 14:03:22 2019

@author: gjoterorodrigu
"""

def velTransVD(u,r,ReTau,mesh):

    import numpy as np
    
    n = mesh.nPoints
    uvd = np.zeros(n)
    ypl = mesh.y*ReTau
    
    for i in range(1,n):
        uvd[i] = uvd[i-1] + np.sqrt(0.5*(r[i]+r[i-1])/r[0])*(u[i]-u[i-1])
        
    return ypl,uvd