"""
   file: HLLC_FLUX
   Toro, pp 331, Eq 10.67-73
"""
import numpy as np
from numpy import pi as pi

class HLLC_FLUX:
    """
    Calculate the flux using Toro, p332, Eqs (10.71) - (10.73)
    For 2D computation notice the following:
    1) The algorithm to find the pressure in the star region (ch4)
       does not depend on the tangential component of velocity.
       This component would appear only in Eqs (4.13) and (4.28c),
       through the total energy terms EL and ER, but these eqs are
       not used to obtain the pressure in the star region
    2) As per the tangential component of velocity, v, we use Eq 9.8,
       p296. See also Fig 9.1
    """
    def __init__(self,dL,uL,vL,pL,dR,uR,vR,pR):
        self.dL,self.uL,self.vL,self.pL = dL,uL,vL,pL
        self.dR,self.uR,self.vR,self.pR = dR,uR,vR,pR
        
    def __call__(self):
        dL,uL,vL,pL = self.dL,self.uL,self.vL,self.pL
        dR,uR,vR,pR = self.dR,self.uR,self.vR,self.pR

        gamma = 1.4
        # initialize
        flux = np.zeros(4)

        # Step I: pressure estimate
        # estimate the pressure and density in the star region
        # using Toro, Eqs (10.67), p331

        aL = np.sqrt(gamma*pL/dL)    # Eq (3.6)
        aR = np.sqrt(gamma*pR/dR)
        abar = 0.5*(aL + aR)
        dbar = 0.5*(dL + dR)
        p_pvrs = 0.5*(pL + pR) - 0.5*(uR - uL)*dbar*abar
        TOL = 1.0e-6
        pstar = max(TOL,p_pvrs)

        # Step II: wave speed estimates

        if pstar <= pL:
            qL = 1.0
        else:
            c1 = 0.5*(gamma + 1.0)/gamma
            qL = np.sqrt(1.0 + c1*(pstar/pL - 1.0))
        if pstar <= pR:
            qR = 1.0
        else:
            c1 = 0.5*(gamma + 1.0)/gamma
            qR = np.sqrt(1.0 + c1*(pstar/pR - 1.0))

        SL = uL - aL*qL
        SR = uR + aR*qR

        # compute now the intermediate speed Sstar using Eq 10.70
        num = pR - pL + dL*uL*(SL - uL) - dR*uR*(SR - uR)
        den = dL*(SL - uL) - dR*(SR - uR)
        Sstar = num/den

        # Step III
        # Compute now the HLLC flux, p 332, Eqs (10.71-73)
        # fluxes in terms of the primitive variables, Eq (10.5),p317
        # for calculation of the tangential speed see Fig 9.1 and Eq (9.8)
        if SL >= 0:
            vt = vL  # tangential component of velocity
            f0 = dL*uL
            f1 = dL*uL**2 + pL
            f2 = dL*uL*vt
            eL = (1.0/(gamma - 1.0))*(pL/dL)    # Eq (3.5)
            EL = dL*(0.5*(uL**2 + vt**2) + eL)  # Eq (3.3)
            f3 = uL*(EL + pL)                # Eq (3.2)
        elif Sstar >= 0:     # SL < 0 <= Sstar
            vt = vL
            f1L = dL*uL
            f2L = dL*uL**2 + pL
            eL = (1.0/(gamma - 1.0))*(pL/dL)    # Eq (3.5)
            EL = dL*(0.5*(uL**2 + vt**2) + eL)  # Eq (3.3)
            f3L = uL*(EL + pL)                  # Eq (3.2)
            d1 = dL*(SL - uL)/(SL - Sstar)
            f0 = f1L + SL*(d1 - dL)
            f1 = f2L + SL*(d1*Sstar - dL*uL)
            f2 = dL*uL*vt
            d2 = EL/dL + (Sstar - uL)*(Sstar + (pL/dL)/(SL - uL))
            f3 = f3L + SL*(d1*d2 - EL)
        elif SR > 0:       # Sstar < 0 < SR:
            vt = vR
            f1R = dR*uR
            f2R = dR*uR**2 + pR
            eR = (1.0/(gamma - 1.0))*(pR/dR)    # Eq (3.5)
            ER = dR*(0.5*(uR**2 + vt**2) + eR)  # Eq (3.3)
            f3R = uR*(ER + pR)                  # Eq (3.2)
            d1 = dR*(SR - uR)/(SR - Sstar)
            f0 = f1R + SR*(d1 - dR)
            f1 = f2R + SR*(d1*Sstar - dR*uR)
            f2 = dR*uR*vt
            d2 = ER/dR + (Sstar - uR)*(Sstar + (pR/dR)/(SR - uR))
            f3 = f3R + SR*(d1*d2 - ER)
        else:   # SR <= 0
            vt = vR
            f0 = dR*uR
            f1 = dR*uR**2 + pR
            f2 = dR*uR*vt
            eR = (1.0/(gamma - 1.0))*(pR/dR)    # Eq (3.5)
            ER = dR*(0.5*(uR**2 + vt**2) + eR)  # Eq (3.3)
            f3 = uR*(ER + pR)                # Eq (3.2)

        flux = np.array([f0,f1,f2,f3])

        # calculate the maximum speed (will determine the next time step)
        Smax = max(abs(SL),abs(SR))

        return flux,Smax
    
