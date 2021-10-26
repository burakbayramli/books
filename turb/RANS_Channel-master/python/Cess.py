#**************************************************************************
#       Implementation of the algebraic Cess model 
#       Reference,
#       Cess, R.D., "A survery of the literature on heat transfer in 
#       turbulent tube flow", Tech. Rep. 8-0529-R24, Westinghouse, 1958. 
#       - Modified for channel flow by: Hussain, A.K.M.F. and Reynolds
#       W.C., "Measurements in fully developed turbulent channel flow",
#       ASME Journal of fluid eng., 1975.
#**************************************************************************
# Cess developed a analytical expression based on the effective viscosity 
# mueff=(mu+ mut). The expression combines the near wall behaviour of the
# eddy viscosity developed by van Driest and the outer layer behaviour 
# from Reichardt.
#
# Conventional model:
#    mueff/mu = 1/2*(1+1/9*kappa^2*ReT^2*(t1*t2)*[1-exp(-yplus/A)]^2)^(1/2) 
#               - 1/2;
#
# Otero et.al model:
#    mueff/mu = 1/2*(1+1/9*kappa^2*Rets^2*(t1*t2)*[1-exp(-ystar/A)]^2)^(1/2) 
#               - 1/2;
# This models uses "yplus" and "ReT". It must be replace by its semi-locally
# scaled counter part "ystar" and "Rets", respectively
#
#
# Input:
#   r           density
#   mu          molecular viscosity
#   ReT         friction Reynolds number ReT=utau r_wall h/ mu_wall
#   mesh        mesh structure
#   compFlag    flag to solve the model with compressible modifications
#
# Output:
#   mut         eddy viscosity or turbulent viscosity
#


def Cess(r,mu,ReTau,mesh,compressibleCorrection):

    import numpy as np

    d = np.minimum(mesh.y, mesh.y[-1]-mesh.y) 

    # Model constants
    kappa   = 0.426
    A       = 25.4

    if compressibleCorrection == 1:
        ReTauArr = np.sqrt(r/r[0])/(mu/mu[0])*ReTau
        yplus = d*ReTauArr
    else: 
        ReTauArr = np.ones(mesh.nPoints)*ReTau
        yplus = d*ReTauArr
        
    df  = 1 - np.exp(-yplus/A)
    t1  = np.power(2*d-d*d, 2)
    t2  = np.power(3-4*d+2*d*d, 2)
    mut = 0.5*np.power(1 + 1/9*np.power(kappa*ReTauArr, 2)*(t1*t2)*df*df, 0.5) - 0.5
    
    return mut*mu






