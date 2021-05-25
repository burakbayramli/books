"""
   file: ANRS_GODUNOV_FLUX

   Note: replaces the EXACT_RS by an adaptive non-interative RS ("ANRS")
         to find the pressure in the star region, pstar. Then proceeds
         as in the EXACT_RS to find the other variables (d,u,e) and
         speeds
   Note: uses a simplified version of EXACT_RS ("ANRS") just to get 
         the Godunov flux on the t-axis (x = 0):
             F[U[x=i+/1/2,t], t > 0    (Toro, Eq (6.12))
   It returns the Godunov flux and the speeds of the Left and Right waves:
        shL, stL, shR, stR
   If a rarefaction wave: sh and st are are speeds of the head and tail of the
   rarefaction wave.
   If a shock wave: sh=st, the speed of the shock wave
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

class Vacuum_Check:
    """
    Check if the initial conditions generate the vacuum state
    """
    def __init__(self,dL,uL,pL,dR,uR,pR):
        # Left and Right states (d,u,p)  (density,speed,pressure)
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        # EOS: ideal gas
        gamma = 1.4
        self.gamma = gamma
        # speed of sound
        self.aL = np.sqrt(gamma*pL/dL)
        self.aR = np.sqrt(gamma*pR/dR)
    def __call__(self):
        gamma = self.gamma
        uL, uR, aL, aR = self.uL, self.uR, self.aL, self.aR
        if uR - uL > 2*aL/(gamma-1) + 2*aR/(gamma - 1):
            # vacuum state: skip the (below) Normal Procedure
            print 'vacuum state: standard solution is not valid'

class Guess_Initial_Pressure:
    """
    Guess an initial value for the pressure using one of the following:
    PVRS: linearized Riemann Solver (RS)
    TRRS: Two-Rarefaction RS
    TSRS: Two-Shock RS
    """
    def __init__(self,dL,uL,pL,dR,uR,pR):
        # Left and Right states (d,u,p)  (density,speed,pressure)
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        # EOS: ideal gas
        gamma = 1.4
        self.gamma = gamma
        # speed of sound
        self.aL = np.sqrt(gamma*pL/dL)
        self.aR = np.sqrt(gamma*pR/dR)
        # Data dependent constants, eq 4.8)
        self.AL = 2.0/((gamma+1)*dL)
        self.AR = 2.0/((gamma+1)*dR)
        self.BL = ((gamma - 1)/(gamma+1))*pL
        self.BR = ((gamma - 1)/(gamma+1))*pR
    def PVRS(self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR, uR, pR = self.dR, self.uR, self.pR
        aL, aR = self.aL, self.aR
        pPV = 0.5*(pL + pR) - 0.125*(uR - uL)*(dL + dR)*(aL + aR)
        TOL = 1.0e-6
        return max(TOL,pPV)
    def TRRS (self):
        gamma = self.gamma
        dL, uL, pL = self.dL, self.uL, self.pL
        dR, uR, pR = self.dR, self.uR, self.pR
        aL, aR = self.aL, self.aR
        ex = (gamma - 1)/(2.0*gamma)
        num = aL + aR - 0.5*(gamma-1)*(uR - uL)
        den = aL/pL**ex + aR/pR**ex
        pTR = (num/den)**(1.0/ex)
        TOL = 1.0e-6
        return max(TOL,pTR)
    def TSRS (self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR, uR, pR = self.dR, self.uR, self.pR
        aL, aR = self.aL, self.aR
        AL, BL, AR, BR = self.AL, self.BL, self.AR, self.BR
        # use the PVRS guess as part of this solution
        pPV = 0.5*(pL + pR) - 0.125*(uR - uL)*(dL + dR)*(aL + aR)
        pPV = max(0,pPV)
        gL = np.sqrt(AL/(pPV + BL))
        gR = np.sqrt(AR/(pPV + BR))
        num = gL*pL + gR*pR - (uR - uL)
        den = gL + gR
        pTS = num/den
        TOL = 1.0e-6
        return max(TOL, pTS)
    def PMEAN (self):
        pL, pR = self.pL, self.pR
        return 0.5*(pL + pR)

class Find_DUE:
    """
    Given the pressure p in the star region calculates the
    density (D), speed (U) and internal energy (E) in the star region
    using ch 4
    Nomenclature used here:
    Input:
        p = pressure in the star region
    Outputs:
        u = speed in the star region
        dLL = density in the star region to the Left of the contact
        dRR = density in the star region to the Right of the contact
        eLL = internal energy in the star region to the Left of the contact
        eRR = internal energy in the star region to the Right of the contact
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,p):
        # Left and Right states (d,u,p)  (density,speed,pressure)
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.p = p  # pressure in the star region
        # EOS: ideal gas
        gamma = 1.4
        self.gamma = gamma
        # speed of sound
        self.aL = np.sqrt(gamma*pL/dL)
        self.aR = np.sqrt(gamma*pR/dR)
        # Data dependent constants, eq 4.8)
        self.AL = 2.0/((gamma+1)*dL)
        self.AR = 2.0/((gamma+1)*dR)
        self.BL = ((gamma - 1)/(gamma+1))*pL
        self.BR = ((gamma - 1)/(gamma+1))*pR
    def __call__(self):
        dL,uL,pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        gamma = self.gamma
        p = self.p
        aL, aR = self.aL, self.aR
        AL,AR,BL,BR = self.AL, self.AR, self.BL, self.BR
        # find the type of L and R waves acoording to the pressure
        # in the L, star and R regions (pL, p, pR)
        pmin = min(pL,pR)
        pmax = max(pL,pR)
        if p < pmin:
            # region I1: two rarefactions
            n = (gamma - 1.0)/(2.0*gamma)
            fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
            fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
            u = 0.5*(uL + uR) + 0.5*(fR - fL)   # Eq (4.9)
            dLL = dL*(p/pL)**(1.0/gamma)        # Eq (4.23)
            dRR = dR*(p/pR)**(1.0/gamma)        # Eq (4.32)
            gan = gamma - 1
            eLL = (p/dLL)/gan   # Eq (1.18)
            eRR = (p/dRR)/gan
        elif p > pmax:
            # region I3 : two shocks
            fL = (p - pL)*np.sqrt(AL/(p + BL))
            fR = (p - pR)*np.sqrt(AR/(p + BR))
            u = 0.5*(uL + uR) + 0.5*(fR - fL)
            ga = (gamma - 1)/(gamma + 1)
            dLL = dL*(p/pL + ga)/(ga*(p/pL) + 1)  # Eq (4.19) from (3.59)
            dRR = dR*(p/pR + ga)/(ga*(p/pR) + 1)  # Eq (3.51)
            gan = gamma - 1
            eLL = (p/dLL)/gan
            eRR = (p/dRR)/gan            
        else:
            # region I2: one rarefaction and one shock
            if pL <= p <= pR:
                # Left shock and Right rarefacion
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (p - pL)*np.sqrt(AL/(p + BL))
                fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
                u = 0.5*(uL + uR) + 0.5*(fR - fL)
                ga = (gamma - 1)/(gamma + 1)
                dLL = dL*(p/pL + ga)/(ga*(p/pL) + 1)
                dRR = dR*(p/pR)**(1.0/gamma)
                gan = gamma - 1
                eLL = (p/dLL)/gan
                eRR = (p/dRR)/gan   
            if pL >= p >= pR:
                # Left rarefaction and Right shock
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
                fR = (p - pR)*np.sqrt(AR/(p + BR))
                u = 0.5*(uL + uR) + 0.5*(fR - fL)
                dLL = dL*(p/pL)**(1.0/gamma)
                ga = (gamma - 1)/(gamma + 1)
                dRR = dR*(p/pR + ga)/(ga*(p/pR) + 1)
                gan = gamma - 1
                eLL = (p/dLL)/gan
                eRR = (p/dRR)/gan   
        return u, dLL, dRR, eLL, eRR

class Find_Front_Speeds:
    """
    Given the pressure p in the star region calculates the
    speed of the shock wave and the head and trail of the
    rarefaction wave
    Nomenclature:
       p = pressure in the star region
       shL, stL: speed of the head and tail of the Left rarefaction
       stR, shR: speed of the tail and head of the Right rarefaction
       sL = speed of the Left shock
       sR = speed of the Right shock
    Note: for simplicity of use the routine returns always 4 values
        for two rarefactions it returns:
           shL, stL, shR, stR
        for two shocks it returns:
           sL, sL, sR, sR
        for Left rarefaction and R shock it returns:
           shL, stL, sR, sR
        and for Left shock and R rarefaction it returns:
           sL, sL, shR, stR
        In the Main program the routine is always called as:
            shL,stL,shR,stR = find_front_speeds()
        So, if shL = stL this means that the Left wave is a shock
        and if shR = stR this means that the Right wave is a shock
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,p):
        # Left and Right states (d,u,p)  (density,speed,pressure)
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.p = p  # pressure in the star region
        # EOS: ideal gas
        gamma = 1.4
        self.gamma = gamma
        # speed of sound
        self.aL = np.sqrt(gamma*pL/dL)
        self.aR = np.sqrt(gamma*pR/dR)
        # Data dependent constants, eq 4.8)
        self.AL = 2.0/((gamma+1)*dL)
        self.AR = 2.0/((gamma+1)*dR)
        self.BL = ((gamma - 1)/(gamma+1))*pL
        self.BR = ((gamma - 1)/(gamma+1))*pR
    def __call__(self):
        dL,uL,pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        gamma = self.gamma
        p = self.p
        aL, aR = self.aL, self.aR
        AL,AR,BL,BR = self.AL, self.AR, self.BL, self.BR
        # find the type of L and R waves acoording to the pressure
        # in the L, star and R regions (pL, p, pR)
        pmin = min(pL,pR)
        pmax = max(pL,pR)
        if p < pmin:
            # region I1: two rarefactions
            n = (gamma - 1.0)/(2.0*gamma)
            fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
            fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
            u = 0.5*(uL + uR) + 0.5*(fR - fL)
            ge = (gamma - 1)/(2.0*gamma)
            aLL = aL*(p/pL)**ge     # Eq (4.25)
            shL = uL - aL
            stL = u - aLL
            aRR = aR*(p/pR)**ge     # Eq (4.34)
            shR = uR + aR
            stR = u + aRR
            return shL, stL, shR, stR
        elif p > pmax:
            # region I3 : two shocks
            fL = (p - pL)*np.sqrt(AL/(p + BL))
            fR = (p - pR)*np.sqrt(AR/(p + BR))
            u = 0.5*(uL + uR) + 0.5*(fR - fL)
            gu1 = (gamma + 1)/(2.0*gamma)
            gu2 = (gamma - 1)/(2.0*gamma)
            sL = uL - aL*np.sqrt(gu1*(p/pL) + gu2)      # Eq (3.62)
            sR = uR + aR*np.sqrt(gu1*(p/pR) + gu2)      # Eq (3.55)
            return sL, sL, sR, sR
        else:
            # region I2: one rarefaction and one shock
            if pL <= p <= pR:
                # Left shock and Right rarefacion
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (p - pL)*np.sqrt(AL/(p + BL))
                fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
                u = 0.5*(uL + uR) + 0.5*(fR - fL)
                gu1 = (gamma + 1)/(2.0*gamma)
                gu2 = (gamma - 1)/(2.0*gamma)
                sL = uL - aL*np.sqrt(gu1*(p/pL) + gu2)
                ge = (gamma - 1)/(2.0*gamma)
                aRR = aR*(p/pR)**ge
                shR = uR + aR
                stR = u + aRR
                return sL, sL, shR, stR
                
            if pL >= p >= pR:
                # Left rarefaction and Right shock
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
                fR = (p - pR)*np.sqrt(AR/(p + BR))
                u = 0.5*(uL + uR) + 0.5*(fR - fL)
                ge = (gamma - 1)/(2.0*gamma)
                aLL = aL*(p/pL)**ge
                shL = uL - aL
                stL = u - aLL
                gu1 = (gamma + 1)/(2.0*gamma)
                gu2 = (gamma - 1)/(2.0*gamma)
                sR = uR + aR*np.sqrt(gu1*(p/pR) + gu2)
                return shL, stL, sR, sR

class P1:
    """
    Case 1: Left rarefaction and Right rarefaction
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,stL,shR,stR):
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.pstar, self.ustar = pstar,ustar
        self.dLL, self.dRR, self.eLL, self.eRR = dLL, dRR, eLL, eRR
        self.shL, self.stL, self.shR, self.stR = shL, stL, shR, stR
        gamma = 1.4
        self.gamma = gamma
    def __call__ (self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, stL, shR, stR = self.shL, self.stL, self.shR, self.stR
        gamma = self.gamma

        # See Toro, Fig 6.5, p219 and Table 6.1, p220
        if ustar > 0:   # cases: a3,a4,a5
            if shL > 0:    # case a3
                d = dL
                u = uL
                p = pL
                e = (p/d)/(gamma - 1)    
            elif stL < 0:  # case a4
                d = dLL
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)    
            else:          # case a5 (inside the Left rarefaction wave)
                # Toro, eq 4.56, p135 with x = 0, t != 0
                aL = np.sqrt(gamma*pL/dL)
                ex1 = 2.0/(gamma - 1)
                ex2 = gamma*ex1
                c1 = 2.0/(gamma + 1)
                c2 = ((gamma - 1)/(gamma + 1))/aL
                c3 = 0.5*(gamma -1)
                d = dL*(c1 + c2*uL)**ex1
                u = c1*(aL + c3*uL)
                p = pL*(c1 + c2*uL)**ex2
                e = (p/d)/(gamma - 1)
        else:   # ustar <=0,  cases b3,b4,b5
            if shR < 0:     # case b3
                d = dR
                u = uR
                p = pR
                e = (p/d)/(gamma - 1)
            elif stR > 0:   # case b4
                d = dRR
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)
            else:           # case b5 (inside the Right rarefaction wave)
                # Toro, eq 4.63, p135 with x = 0 t != 0
                aR = np.sqrt(gamma*pR/dR)
                ex1 = 2.0/(gamma - 1)
                ex2 = gamma*ex1
                c1 = 2.0/(gamma + 1)
                c2 = ((gamma - 1)/(gamma + 1))/aR
                c3 = 0.5*(gamma -1)
                d = dR*(c1 - c2*uR)**ex1
                u = c1*(-aR + c3*uR)
                p = pR*(c1 - c2*uR)**ex2
                e = (p/d)/(gamma - 1)                
        # finally ...
        return d,u,p,e
        
class P2:
    """
    Case 2: Left rarefaction and Right shock
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,stL,shR):
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.pstar, self.ustar = pstar,ustar
        self.dLL, self.dRR, self.eLL, self.eRR = dLL, dRR, eLL, eRR
        self.shL, self.stL, self.shR = shL, stL, shR
        gamma = 1.4
        self.gamma = gamma
    def __call__ (self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, stL, shR = self.shL, self.stL, self.shR
        gamma = self.gamma

        if ustar > 0:   # cases a3,a4,a5
            if shL > 0:    # case a3
                d = dL
                u = uL
                p = pL
                e = (p/d)/(gamma - 1)    
            elif stL < 0:  # case a4
                d = dLL
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)    
            else:          # case a5 (inside the Left rarefaction wave)
                # Toro, eq 4.56, p135 with x = 0, t != 0
                aL = np.sqrt(gamma*pL/dL)
                ex1 = 2.0/(gamma - 1)
                ex2 = gamma*ex1
                c1 = 2.0/(gamma + 1)
                c2 = ((gamma - 1)/(gamma + 1))/aL
                c3 = 0.5*(gamma -1)
                d = dL*(c1 + c2*uL)**ex1
                u = c1*(aL + c3*uL)
                p = pL*(c1 + c2*uL)**ex2
                e = (p/d)/(gamma - 1)
        else:   # ustar <=0,  cases b1,b2
            if shR < 0:     # case b1
                d = dR
                u = uR
                p = pR
                e = (p/d)/(gamma - 1)
            else:   # shR >= 0,  case b2
                d = dRR
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)
        # finally ...
        return d,u,p,e
        
class P3:
    """
    Case 3: Left shock and Right rarefaction
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR,stR):
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.pstar, self.ustar = pstar,ustar
        self.dLL, self.dRR, self.eLL, self.eRR = dLL, dRR, eLL, eRR
        self.shL, self.shR, self.stR = shL, shR, stR
        gamma = 1.4
        self.gamma = gamma
    def __call__ (self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, shR, stR = self.shL, self.shR, self.stR
        gamma = self.gamma

        if ustar > 0:   # cases a1, a2
            if shL > 0:     # case a1
                d = dL
                u = uL
                p = pL
                e = (p/d)/(gamma - 1)
            else:  # shL <=0, case a2
                d = dLL
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)
        else:  # ustar <=0  cases b3,b4,b5
            if shR < 0:     # case b3
                d = dR
                u = uR
                p = pR
                e = (p/d)/(gamma - 1)
            elif stR > 0:   # case b4
                d = dRR
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)
            else:           # case b5 (inside the Right rarefaction wave)
                # Toro, eq 4.63, p135 with x = 0 t != 0
                aR = np.sqrt(gamma*pR/dR)
                ex1 = 2.0/(gamma - 1)
                ex2 = gamma*ex1
                c1 = 2.0/(gamma + 1)
                c2 = ((gamma - 1)/(gamma + 1))/aR
                c3 = 0.5*(gamma -1)
                d = dR*(c1 - c2*uR)**ex1
                u = c1*(-aR + c3*uR)
                p = pR*(c1 - c2*uR)**ex2
                e = (p/d)/(gamma - 1)                        
        # finally ...
        return d,u,p,e
        
class P4:
    """
    Case 4: Left shock and Right shock
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR):
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.pstar, self.ustar = pstar,ustar
        self.dLL, self.dRR, self.eLL, self.eRR = dLL, dRR, eLL, eRR
        self.shL, self.shR = shL, shR
        gamma = 1.4
        self.gamma = gamma
    def __call__ (self):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, shR = self.shL, self.shR
        gamma = self.gamma

        if ustar > 0:    # cases a1,a2
            if shL > 0:     # case a1
                d = dL
                u = uL
                p = pL
                e = (p/d)/(gamma - 1)
            else:  # shL <= 0,  case a2
                d = dLL
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)
        else:   # ustar <=0   cases b1,b2
            if shR < 0:     # case b1
                d = dR
                u = uR
                p = pR
                e = (p/d)/(gamma - 1)
            else:   # shR >=0 case b2
                d = dRR
                u = ustar
                p = pstar
                e = (p/d)/(gamma - 1)    
        # finally ...
        return d,u,p,e

class ANRS:
    """
    If Ispeed = 1, it will return also the speeds of the Left and Right
    front waves:
        shL: Left wave, speed of its head
        stL: Left wave, speed of its head
        shR: Right wave, speed of its head
        stR: Right wave, speed of its tail.
        If a wave is a shock, then speed of the tail = speed of the head
    If Ispeed = 0, it will only return the following:
        d: density  at x = 0
        u: normal component of speed at x = 0
        p: pressure at x = 0
        e: internal energy at x = 0      
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,Ispeed=0):
        self.dL,self.uL,self.pL = dL,uL,pL
        self.dR,self.uR,self.pR = dR,uR,pR
        self.Ispeed = Ispeed
    def __call__(self):
        dL,uL,pL = self.dL,self.uL,self.pL
        dR,uR,pR = self.dR,self.uR,self.pR
        Ispeed = self.Ispeed
        
        gamma = 1.4
        v = Vacuum_Check(dL,uL,pL,dR,uR,pR)
        v()         
        g = Guess_Initial_Pressure(dL,uL,pL,dR,uR,pR)
        p0 = g.PVRS()
        # find the pressure in the star region non-iteratively
        # Replace the iterative exact RS by the ANRS  (p 306, Fig 9.4)
        # decide whether to use PVRS, TRRS or TSRS     
        Q = 2.0
        pmin = min(pL,pR)
        pmax = max(pR,pR)
        if pmax/pmin < Q and pmin < p0 < pmax:
            # use the PVRS solver
            pstar = p0
        elif p0 > pmin:
            # use the TSRS solver
            # TSRS
            AL = 2.0/((gamma+1)*dL)
            AR = 2.0/((gamma+1)*dR)
            BL = ((gamma - 1)/(gamma+1))*pL
            BR = ((gamma - 1)/(gamma+1))*pR
            gL = np.sqrt(AL/(p0 + BL))
            gR = np.sqrt(AR/(p0 + BR))
            pstar = (gL*pL + gR*pR - (uR - uL))/(gL + gR)  # Eq 9.42
        else:  # p0 < pmin
            # use the TRRS solver
            aL = np.sqrt(gamma*pL/dL)
            aR = np.sqrt(gamma*pR/dR)
            ex = (gamma - 1)/(2.0*gamma)
            num = aL + aR - 0.5*(gamma-1)*(uR - uL)
            den = aL/pL**ex + aR/pR**ex
            pstar = (num/den)**(1.0/ex)
        TOL = 1.0e-6
        pstar = max(TOL,pstar)  # return a non-negative pressure

        # find the density, speed and energy in the star region
        due = Find_DUE(dL,uL,pL,dR,uR,pR,pstar)
        ustar, dLL, dRR, eLL, eRR = due()

        find_front_speeds = Find_Front_Speeds(dL,uL,pL,dR,uR,pR,pstar)
        shL,stL,shR,stR = find_front_speeds()

        # find out which one of the 4 possible cases the solution belongs to:
        # case 1: Left rarefaction and Right rarefaction
        # case 2: Left rarefaction and Right shock
        # case 3: Left shock and Right rarefaction
        # case 4: Left shock and Right shock
        # and accordingly call different functions and calculate W(x,t)
        if pstar <= pL and pstar <= pR:
            p1 = P1(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,stL,shR,stR)
            d,u,p,e = p1()  # calculate W(x=0,t)
        if pstar <= pL and pstar > pR:
            p2 = P2(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,stL,shR)
            d,u,p,e = p2()  # calculate W(x=0,t) 
        if pstar > pL and pstar <= pR:
            p3 = P3(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR,stR)
            d,u,p,e = p3()  # calculate W(x=0,t) 
        if pstar > pL and pstar > pR:
            p4 = P4(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR)
            d,u,p,e = p4()  # calculate W(x=0,t)
        if Ispeed == 0:
            return d,u,p,e
        if Ispeed == 1: # returns also the speeds of the Left and Right waves
            return d,u,p,e,shL,stL,shR,stR

class ANRS_GODUNOV_FLUX:
    """
    Calculate the flux using Fig 6.5, p 219 and Table 6.1, p 220
    with the exact Riemann Solver (EXACT_RS)
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

        ex = ANRS(dL,uL,pL,dR,uR,pR,Ispeed=1)
        d,u,p,e,shL,stL,shR,stR = ex()

        w1 = d
        w2 = u
        w3 = p
        toL = 0.01*max(abs(w2),abs(uL))
        toR = 0.01*max(abs(w2),abs(uR))
        # tangential component of velocity
        if abs(w2 - uL) < toL:
            vt = vL
        elif abs(w2 - uR) < toR:
            vt = vR
        elif w2 > 0:
            vt = vL
        else:
            vt = vR

        # In the Godunov method we are only interested in calculating
        # W(x=0,t), which gives rise to 10 cases (p 219, Fig 6.5) and
        # 10 possible values for W(x=0,t) (p 220, Table 6.1)

        # fluxes in terms of the primitive variables
        # p 220, Eq following Table 6.1, with flux given by Eq (9.5)
        flux[0] = w1*w2
        flux[1] = w1*w2**2 + w3
        flux[2] = w1*w2*vt
        e = (1.0/(gamma - 1))*(w3/w1)
        E = w1*(e + 0.5*(w2**2 + vt**2))  # Eq (3.3), p88, for 2D
        flux[3] = w2*(E + w3)

        # calculate the maximum speed (will determine the next time step)
        Smax = max(abs(shL),abs(stL),abs(shR),abs(stR))

        return flux,Smax
    
