#**************************************************************************
#       Implementation of k-omega SST
#       Reference,
#       Menter, F.R., "Zonal Two equation k-omega turbulence models for 
#       aerodynamic flows", AIAA 93-2906, 1993.
#**************************************************************************
# Two equation turbulence model, which combines Wilcox k-omega model
# and k-epsilon mode through a blending funcion
#
# Conventional models without compressible modifications:
#    k-eq:  0 = Pk - (beta_star rho k om) + ddy[(mu+mut*sigma_k) dkdy]
#    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
#               + ddy[(mu+mut*sigma_om)domdy] + (1-BF1) CDkom
#
# Otero et.al model:
#    k-eq:  0 = Pk - (beta_star rho k om) 
#               + 1/sqrt(rho) ddy[1/sqrt(rho) (mu+mut*sigma_k) d(rho k)dy]
#    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
#               + ddy[1/sqrt(rho) (mu+mut*sigma_om)d(sqrt(rho) om)dy] 
#               + (1-BF1) CDkom_mod
#
# Catris, S. and Aupoix, B., "Density corrections for turbulence
#       models", Aerosp. Sci. Techn., 2000.
#    k-eq:  0 = Pk - (beta_star rho k om) 
#               + ddy[1/rho (mu+mut*sigma_k) d(rho k)dy]
#    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
#               + ddy[1/sqrt(rho) (mu+mut*sigma_om)d(sqrt(rho) om)dy] 
#               + (1-BF1) CDkom
#
# For simplicty, the extra density factors of the Otero et.al and Catris/Aupoix  
# models were implmeneted as extra source terms. Therefore what is solved is:
#    k-eq:  0 = Pk - (beta_star rho k om) + ddy[(mu+mut*sigma_k) dkdy]
#               + Source
#    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
#               + ddy[(mu+mut*sigma_om)domdy] + (1-BF1) CDkom + Source
#
# Input:
#   u           velocity
#   k           turbulent kinetic energy, from previous time step
#   om          turbulent kinetic energy dissipation rate, from previous 
#               time step
#   r           density
#   mu          molecular viscosity
#   mesh        mesh structure
#   compFlag    flag to solve the model with compressible modifications
#
# Output:
#   mut         eddy viscosity or turbulent viscosity
#   k           turbulent kinetic energy
#   om          turbulent kinetic energy dissipation rate



def SST(u,k,om,r,mu,mesh,compressibleCorrection):
    
    import numpy as np
    from solveEqn import solveEqn
    
    n = mesh.nPoints

    # model constants
    sigma_k1  = 0.85
    sigma_k2  = 1.0
    sigma_om1 = 0.5
    sigma_om2 = 0.856
    beta_1    = 0.075
    beta_2    = 0.0828
    betaStar  = 0.09
    a1        = 0.31
    alfa_1    = beta_1/betaStar - sigma_om1*0.41**2.0/betaStar**0.5
    alfa_2    = beta_2/betaStar - sigma_om2*0.41**2.0/betaStar**0.5    
    
    # Relaxation factors
    underrelaxK  = 0.6
    underrelaxOm = 0.4
        
    # required gradients
    dkdy  = mesh.ddy@k
    domdy = mesh.ddy@om
    
    wallDist = np.minimum(mesh.y, mesh.y[-1]-mesh.y) 
    wallDist = np.maximum(wallDist, 1.0e-8)


    # VortRate = StrainRate in fully developed channel
    strMag = np.absolute(mesh.ddy@u) 
    
    # Blending functions 
    CDkom  = 2.0*sigma_om2*r/om*dkdy*domdy
    gamma1 = 500.0*mu/(r*om*wallDist*wallDist)
    gamma2 = 4.0*sigma_om2*r*k/(wallDist*wallDist*np.maximum(CDkom,1.0e-20))
    gamma3 = np.sqrt(k)/(betaStar*om*wallDist)
    gamma  = np.minimum(np.maximum(gamma1, gamma3), gamma2)
    bF1    = np.tanh(np.power(gamma, 4.0))
    gamma  = np.maximum(2.0*gamma3, gamma1)
    bF2    = np.tanh(np.power(gamma, 2.0))

    # more model constants
    alfa     = alfa_1*bF1    + (1-bF1)*alfa_2
    beta     = beta_1*bF1    + (1-bF1)*beta_2
    sigma_k  = sigma_k1*bF1  + (1-bF1)*sigma_k2
    sigma_om = sigma_om1*bF1 + (1-bF1)*sigma_om2
    
    # Eddy viscosity
    zeta = np.minimum(1.0/om, a1/(strMag*bF2))
    mut = r*k*zeta
    mut = np.minimum(np.maximum(mut,0.0),100.0)
    
    # ---------------------------------------------------------------------
    # om-equation
    
    # effective viscosity
    if compressibleCorrection == 1:
        mueff = (mu + sigma_om*mut)/np.sqrt(r)
        fs    = np.sqrt(r)
    else:
        mueff = mu + sigma_om*mut
        fs    = np.ones(n)

    # diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A = np.einsum('i,ij->ij', mueff, mesh.d2dy2) \
      + np.einsum('i,ij->ij', mesh.ddy@mueff, mesh.ddy)
    
    # implicitly treated source term
    np.fill_diagonal(A, A.diagonal() - beta*r*om/fs)

    # Right-hand-side
    b = -alfa[1:-1]*r[1:-1]*strMag[1:-1]*strMag[1:-1] - (1-bF1[1:-1])*CDkom[1:-1]
    
    # Wall boundary conditions
    om[0 ] = 60.0*mu[0 ]/beta_1/r[0 ]/wallDist[1 ]/wallDist[1 ]
    om[-1] = 60.0*mu[-1]/beta_1/r[-1]/wallDist[-2]/wallDist[-2]

    # Solve
    om = solveEqn(om*fs, A, b, underrelaxOm)/fs
    om[1:-1] = np.maximum(om[1:-1], 1.e-12)
    
    # ---------------------------------------------------------------------
    # k-equation    
    
    # effective viscosity
    if compressibleCorrection == 1:
        mueff = (mu + sigma_k*mut)/np.sqrt(r)
        fs    = r
        fd    = np.sqrt(r)
    else:
        mueff = mu + sigma_k*mut
        fs    = np.ones(n)
        fd    = np.ones(n)

    # diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A = np.einsum('i,ij->ij', mueff*fd, mesh.d2dy2) \
      + np.einsum('i,ij->ij', (mesh.ddy@mueff)*fd, mesh.ddy)

    # implicitly treated source term
    np.fill_diagonal(A, A.diagonal() - betaStar*r*om/fs)

    # Right-hand-side
    Pk = np.minimum(mut*strMag*strMag, 20*betaStar*k*r*om)
    b  = -Pk[1:-1]
    
    # Wall boundary conditions
    k[0] = k[-1] = 0.0
    
    # Solve
    k = solveEqn(k*fs, A, b, underrelaxK)/fs
    k[1:-1] = np.maximum(k[1:-1], 1.e-12)

    return mut,k,om
