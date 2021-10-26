#**************************************************************************
#       function to solve linear system  [A].x = b
#**************************************************************************
# Inputs:
#   x       variable to be solved
#   A       coefficient matrix
#   b       right hand side
#   omega   under-relaxation parameter
#
# Output:
#   x_new   updated x
#


def solveEqn(x,A,b,omega):

    import numpy as np

    n = np.size(x)
    x_new = x.copy()
    
    # add boundary conditions
    b = b - x[0]*A[1:n-1,0] - x[n-1]*A[1:n-1,n-1]
    
    # perform under-relaxation
    b[:] = b[:] + (1-omega)/omega * A.diagonal()[1:-1]*x[1:-1]
    np.fill_diagonal(A, A.diagonal()/omega)
    
    # solve linear system
    x_new[1:-1] = np.linalg.solve(A[1:-1, 1:-1], b)
    return x_new
    
def solveRANS(r_DNS,mu_DNS): #(mesh,r_DNS,mu_DNS,compressCorrection,solveTemperatureEq,Pr,ReTau,expLam,expRho,expMu,Qvol):

    import numpy as np
    
    n = mesh.nPoints
    
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
            r, mu, T   = solveTemperature(T,r,mu,mut,mesh,Pr,ReTau,expLam,expRho,expMu,Qvol)

        # Solve turbulence model to calculate eddy viscosity 
        if   turbModel == "Cess":   mut        = Cess(r,mu,ReTau,mesh,compressCorrection)
        elif turbModel == "SA":     mut,nuSA   = SA(u,nuSA,r,mu,mesh,compressCorrection)
        elif turbModel == "MK":     mut,k,e    = MK(u,k,e,r,mu,ReTau,mesh,compressCorrection)
        elif turbModel == "SST":    mut,k,om   = SST(u,k,om,r,mu,mesh,compressCorrection)
        elif turbModel == "V2F":    mut,k,e,v2 = V2F(u,k,e,v2,r,mu,mesh,compressCorrection)
        else:                       mut        = np.zeros(n)

        # Solve momentum equation:  0 = d/dy[(mu+mut)dudy] - 1
        # diffusion matrix: mueff*d2phi/dy2 + dmueff/dy dphi/dy    
        A = np.einsum('i,ij->ij', mesh.ddy@(mu + mut), mesh.ddy) \
          + np.einsum('i,ij->ij', mu + mut, mesh.d2dy2)

        # Solve 
        u_old = u.copy()
        u[1:n-1] = np.linalg.solve(A[1:n-1, 1:n-1], -np.ones(n-2))
        residual = np.linalg.norm(u-u_old)/n

        # Printing residuals
        if iterations%100 == 0: print("iteration: ",iterations, ", Residual(u) = ", residual)
        iterations = iterations + 1

    print("iteration: ",iterations, ", Residual(u) = ", residual)
    
    return u, T, r, mu, mut, k, e, om
    
def solveTemperature(T,r,mu,mut): #,mesh,Pr,ReTau,expLam,expRho,expMu,Qvol):
    
    import numpy as np
        
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