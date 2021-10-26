# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 14:15:36 2019

@author: gjoterorodrigu
"""

def solveTemperature(T,r,mut,mesh,Pr,ReTau,expLam,expRho,expMu,Qvol):
    
    import numpy as np
    from solveEqn import solveEqn    
    n = mesh.nPoints
    
    # molecular thermal conductivity: 
    lam = np.power(T, expLam)/(ReTau*Pr)   
    
    # turbulent Prandtl: assume = 1
    Prt = np.ones(n)                          

    # diffusion matrix: lamEff*d2phi/dy2 + dlamEff/dy dphi/dy
    A = np.einsum('i,ij->ij', mesh.ddy@(lam + mut/Prt), mesh.ddy) + np.einsum('i,ij->ij', lam + mut/Prt, mesh.d2dy2)

    # Isothermal BC
    T[0] = T[-1] = 1              
    
    # source term
    b = -Qvol*np.ones(n-2)/(ReTau*Pr)

    # Solve
    T = solveEqn(T,A,b,0.95)

    # calculate density and viscosity from temperature
    r  = np.power(T, expRho)
    mu = np.power(T, expMu)/ReTau
    
    return r, mu, T
