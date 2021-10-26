# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 14:14:16 2019

@author: gjoterorodrigu
"""

def solveRANS(r_DNS,mu_DNS,mesh,turbModel,compressCorrection,solveTemperatureEq,Pr,ReTau,expLam,expRho,expMu,Qvol):

    import numpy as np
    from solveTemperature import solveTemperature
    from Cess import Cess
    from SA import SA
    from MK import MK
    from SST import SST
    from V2F import V2F

    n    = mesh.nPoints
    u    = np.zeros(n)          # velocity 
    T    = np.ones(n)           # temperature 
    mut  = np.zeros(n)          # eddy viscosity 
    
    r    = r_DNS.copy()
    mu   = mu_DNS.copy()
    
    k    = 0.01*np.ones(n)      # turbulent kinetic energy
    e    = 0.001*np.ones(n)     # turbulent dissipation 
    v2   = 1/3*k                # wall normal turbulent fluctuations for V2F model
    om   = np.ones(n)           # specific turbulent dissipation for omega in SST
    nuSA = np.ones(n)/ReTau     # eddy viscisty for SA model
    
    residual   = 1.0e20
    iterations = 0
    
    print("Start iterating")

    while residual > 1.0e-6 and iterations < 10000:

        # Solve temperature:  d/dy[(lam+mut/PrT)dTdy] = -VolQ/ReTau/Pr
        if solveTemperatureEq == 1:       
            r, mu, T   = solveTemperature(T,r,mut,mesh,Pr,ReTau,expLam,expRho,expMu,Qvol)

        # Solve turbulence model to calculate eddy viscosity 
        if   turbModel == "Cess":   mut        = Cess(r,mu,ReTau,mesh,compressCorrection)
        elif turbModel == "SA":     mut,nuSA   = SA(u,nuSA,r,mu,mesh,compressCorrection)
        elif turbModel == "MK":     mut,k,e    = MK(u,k,e,r,mu,ReTau,mesh,compressCorrection)
        elif turbModel == "SST":    mut,k,om   = SST(u,k,om,r,mu,mesh,compressCorrection)
        elif turbModel == "V2F":    mut,k,e,v2 = V2F(u,k,e,v2,r,mu,mesh,compressCorrection)
        else:                       mut        = np.zeros(n)

        # Solve momentum equation:  0 = d/dy[(mu+mut)dudy] - 1
        # diffusion matrix: mueff*d2phi/dy2 + dmueff/dy dphi/dy    
        A = np.einsum('i,ij->ij', mesh.ddy@(mu + mut), mesh.ddy) + np.einsum('i,ij->ij', mu + mut, mesh.d2dy2)

        # Solve 
        u_old = u.copy()
        u[1:n-1] = np.linalg.solve(A[1:n-1, 1:n-1], -np.ones(n-2))
        residual = np.linalg.norm(u-u_old)/n

        # Printing residuals
        if iterations%100 == 0: print("iteration: ",iterations, ", Residual(u) = ", residual)
        iterations = iterations + 1

    print("iteration: ",iterations, ", Residual(u) = ", residual)
    
    return u, T, r, mu, mut, k, e, om
