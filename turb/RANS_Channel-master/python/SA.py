#**************************************************************************
#       Implementation of SA model (Spalart-Allmaras 1992 AIAA)
#       Reference,
#       Spalart, A. and Allmaras, S., "One equation turbulence model for 
#       aerodynamic flows", Recherche Aerospatiale-French edition, 1994.
#**************************************************************************
# The SA model consists of a transport equation for an eddy viscosity-like
# scalar derived using dimensional analysis, Galilean invariance and
# empiricism.
#
# Conventional models without compressible modifications:
#    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
#                  + 1/cb3 ddy[(nu+nuSA) dnuSAdy] + cb2/cb3 (dnuSAdy)^2
#
# Otero et.al model:
#    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
#                  + 1/cb3 1/rho ddy[rho (nu+nuSA) dnuSAdy]
#                  + 1/cb3 1/rho ddy[nuSA/2 (nu+nuSA) drhody]
#                  + cb2/cb3 1/rho (d(sqrt(rho) nuSA)dy)^2
#
# Catris, S. and Aupoix, B., "Density corrections for turbulence
#       models", Aerosp. Sci. Techn., 2000.
#    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
#                  + 1/cb3 1/rho ddy[rho (nu+nuSA) dnuSAdy]
#                  + 1/cb3 1/rho ddy[nuSA/2 (nuSA) drhody]
#                  + cb2/cb3 1/rho (d(sqrt(rho) nuSA)dy)^2
#
# For simplicty, the extra density factors of the Otero et.al and Catris/Aupoix
# models were implmeneted as extra source terms. Therefore what is solved is:
# Conventional model:
#    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
#                  + 1/cb3 ddy[(nu+nuSA) dnuSAdy] + cb2/cb3 (dnuSAdy)^2
#                  + Source
#
# Input:
#   u           velocity
#   nuSA        eddy viscosity-like scalar, from previous time step
#   r           density
#   mu          molecular viscosity
#   mesh        mesh structure
#   compFlag    flag to solve the model with compressible modifications
#
# Output:
#   mut         eddy viscosity or turbulent viscosity
#   nuSA        solved eddy viscosity-like scalar

def SA(u,nuSA,r,mu,mesh,compressibleCorrection):
    
    import numpy as np
    from solveEqn import solveEqn
    
    n = mesh.nPoints

    # Model constants
    cv1_3   = np.power(7.1, 3.0)
    cb1     = 0.1355
    cb2     = 0.622
    cb3     = 2.0/3.0
    inv_cb3 = 1.0/cb3
    kappa_2 = np.power(0.41, 2.0)
    cw1     = cb1/kappa_2 + (1.0+cb2)/cb3
    cw2     = 0.3
    cw3_6   = np.power(2.0, 6.0)
    
    # Model functions
    strMag        = np.absolute(mesh.ddy@u)   # VortRate = StrainRate in fully developed channel
    wallDist      = np.minimum(mesh.y, mesh.y[-1]-mesh.y) 
    wallDist      = np.maximum(wallDist, 1.0e-8)
    inv_wallDist2 = 1/np.power(wallDist, 2)

    chi           = nuSA*r/mu
    fv1           = np.power(chi, 3)/(np.power(chi, 3) + cv1_3)
    fv2           = 1.0 - (chi/(1.0+(chi*fv1)))
    inv_kappa2_d2 = inv_wallDist2*(1.0/kappa_2)
    Shat          = strMag + inv_kappa2_d2*fv2*nuSA
    inv_Shat      = 1.0/Shat
    r_SA          = np.minimum(nuSA*inv_kappa2_d2*inv_Shat, 10.0)
    g             = r_SA + cw2*(np.power(r_SA, 6) - r_SA)
    g_6           = np.power(g, 6)
    fw_           = np.power(((1.0 + cw3_6)/(g_6+ cw3_6)), (1/6))
    fw            = g*fw_

    # Eddy viscosity
    mut       = np.zeros(n)
    mut[1:-1] = fv1[1:-1]* nuSA[1:-1]*r[1:-1]
    mut[1:-1] = np.minimum(np.maximum(mut[1:-1], 0.0), 100.0)
    
    if compressibleCorrection == 1:  
        nueff = (mu/r + nuSA)*r
        fs    = np.sqrt(r)
        fd    = 1/r
        drdy  = mesh.ddy@r
        Di    = nueff*nuSA*drdy
        drho  = 0.5*inv_cb3/r*(mesh.ddy@Di)
    else:
        nueff = (mu/r + nuSA)
        fs    = np.ones(n)
        fd    = np.ones(n)
        drho  = np.zeros(n)

    # ---------------------------------------------------------------------
    # nuSA-equation 
    
    # diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A = np.einsum('i,ij->ij', nueff*fd, mesh.d2dy2) \
      + np.einsum('i,ij->ij', (mesh.ddy@nueff)*fd, mesh.ddy)
    A = inv_cb3*A
    
    # implicitly treated source term
    np.fill_diagonal(A, A.diagonal() - cw1*fw*nuSA*inv_wallDist2)

    # Right hand side
    dnudy = mesh.ddy@(fs*nuSA)
    b  = - cb1*Shat[1:-1]*nuSA[1:-1] - cb2*inv_cb3*np.power(dnudy[1:-1], 2) - drho[1:-1]
    
    # Wall boundary conditions
    nuSA[0] = nuSA[-1] = 0.0

    # Solve
    nuSA = solveEqn(nuSA, A, b, 0.75)
    nuSA[1:-1] = np.maximum(nuSA[1:-1], 1.e-12)

    return mut,nuSA


