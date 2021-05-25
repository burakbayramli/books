"""
   Usage: run EXACT_RS
   
   Includes MAIN inside:
      if __name__ == '__main__':
   the module can be either run as an usual module
   (using: run EXACT_RS) or imported and used in another
   module (skipping the '__main__' section)

   If it runs as a usual module it simulates the 5 tests in Toro, p225

   Find and plot the solution for a given time t > 0
   
   Finds the front wave speeds corresponding to Table 4.3, p131, Toro
   Exact Riemann solver: given the initial Left and Right states
       find the pressure in the 'star' region. Then, calculate the
       front wave speeds for the L and R waves.
       If a rarefaction wave it returns (SH,ST), the speed of the
       head and the speed of the tail.
       If a shock wave it returns (SS,SS), the speed of the shock wave
       (twice the same number)
"""
import numpy as np
from numpy import pi as pi
from scipy.optimize import brentq
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

class Find_Pressure:
    """
    implementation of LHS of eq (4.5)
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,p0):
        # Left and Right states (d,u,p)  (density,speed,pressure)
        self.dL, self.uL, self.pL = dL,uL,pL
        self.dR, self.uR, self.pR = dR,uR,pR
        self.p0 = p0  # initial guess for the pressure
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
    def __call__(self,p):
        dL,uL,pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        p0 = self.p0
        gamma = self.gamma
        aL, aR = self.aL, self.aR
        AL,AR,BL,BR = self.AL, self.AR, self.BL, self.BR
        # find in which region the initial guess is: I1, I2 or I3
        pmin = min(pL,pR)
        pmax = max(pL,pR)
        if p0 < pmin:
            # region I1: two rarefactions
            n = (gamma - 1.0)/(2.0*gamma)
            fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
            fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
        elif p0 > pmax:
            # region I3 : two shocks
            fL = (p - pL)*np.sqrt(AL/(p + BL))
            fR = (p - pR)*np.sqrt(AR/(p + BR))
        else:
            # region I2: one rarefaction and one shock
            if pL <= p0 <= pR:
                # Left shock and Right rarefacion
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (p - pL)*np.sqrt(AL/(p + BL))
                fR = (2.0*aR/(gamma-1))*((p/pR)**n - 1.0)
            if pL >= p0 >= pR:
                # Left rarefaction and Right shock
                n = (gamma - 1.0)/(2.0*gamma)
                fL = (2.0*aL/(gamma-1))*((p/pL)**n - 1.0)
                fR = (p - pR)*np.sqrt(AR/(p + BR))
        return fL + fR + (uR - uL)

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
    def __call__ (self, t):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, stL, shR, stR = self.shL, self.stL, self.shR, self.stR
        gamma = self.gamma
        # find the positions of the Left, contact and Right fronts
        xhL = shL*t
        xtL = stL*t
        xc  = ustar*t
        xtR = stR*t
        xhR = shR*t
        # define the x domain to include all the relevant fronts
        xL = xhL - 0.1*np.abs(xhR - xhL)
        xR = xhR + 0.1*np.abs(xhR - xhL)
        # define the variables in [xL,xR]
        # x,d,u,p,e = p1(t)
        x = np.linspace(xL,xR,201)
        d = np.zeros(len(x))
        u = np.zeros(len(x))
        p = np.zeros(len(x))
        e = np.zeros(len(x))
        # calculate d, u, p and e in the different regions
        # to the left of the Left rarefaction wave
        ind = np.where(x <= xhL)
        d[ind] = dL
        u[ind] = uL
        p[ind] = pL
        e[ind] = (pL/dL)/(gamma - 1)
        # inside the Left rarefaction
        ind = np.logical_and(x > xhL, x <= xtL)
        # Toro, eq 4.56, p135
        aL = np.sqrt(gamma*pL/dL)
        ex1 = 2.0/(gamma - 1)
        ex2 = gamma*ex1
        c1 = 2.0/(gamma + 1)
        c2 = ((gamma - 1)/(gamma + 1))/aL
        c3 = 0.5*(gamma -1)
        d[ind] = dL*(c1 + c2*(uL - x[ind]/t))**ex1
        u[ind] = c1*(aL + c3*uL + x[ind]/t)
        p[ind] = pL*(c1 + c2*(uL - x[ind]/t))**ex2
        e[ind] = (p[ind]/d[ind])/(gamma - 1)
        # between the tail of the Left rarefaction and the contact disc
        ind = np.logical_and(x > xtL, x <= xc)
        d[ind] = dLL
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eLL
        # between the contact disc and the tail of the Right rarefaction
        ind = np.logical_and(x > xc, x <= xtR)
        d[ind] = dRR
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eRR
        # inside the Right rarefaction
        ind = np.logical_and(x > xtR, x <= xhR)
        # Toro, eq 4.63, p135
        aR = np.sqrt(gamma*pR/dR)
        ex1 = 2.0/(gamma - 1)
        ex2 = gamma*ex1
        c1 = 2.0/(gamma + 1)
        c2 = ((gamma - 1)/(gamma + 1))/aR
        c3 = 0.5*(gamma -1)
        d[ind] = dR*(c1 - c2*(uR - x[ind]/t))**ex1
        u[ind] = c1*(-aR + c3*uR + x[ind]/t)
        p[ind] = pR*(c1 - c2*(uR - x[ind]/t))**ex2
        e[ind] = (p[ind]/d[ind])/(gamma - 1)
        # to the right of the Right rarefaction
        ind = np.where(x > xhR)
        d[ind] = dR
        u[ind] = uR
        p[ind] = pR
        e[ind] = (pR/dR)/(gamma - 1)
        # finally ...
        return x,d,u,p,e
        
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
    def __call__ (self, t):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, stL, shR = self.shL, self.stL, self.shR
        gamma = self.gamma
        # find the positions of the Left, contact and Right fronts
        xhL = shL*t
        xtL = stL*t
        xc  = ustar*t
        xhR = shR*t
        # define the x domain to include all the relevant fronts
        xL = xhL - 0.1*np.abs(xhR - xhL)
        xR = xhR + 0.1*np.abs(xhR - xhL)
        # define the variables in [xL,xR]
        # x,d,u,p,e = p1(t)
        x = np.linspace(xL,xR,201)
        d = np.zeros(len(x))
        u = np.zeros(len(x))
        p = np.zeros(len(x))
        e = np.zeros(len(x))
        # calculate d, u, p and e in the different regions
        # to the left of the Left rarefaction wave
        ind = np.where(x <= xhL)
        d[ind] = dL
        u[ind] = uL
        p[ind] = pL
        e[ind] = (pL/dL)/(gamma - 1)
        # inside the Left rarefaction
        ind = np.logical_and(x > xhL, x <= xtL)
        # Toro, eq 4.56, p135
        aL = np.sqrt(gamma*pL/dL)
        ex1 = 2.0/(gamma - 1)
        ex2 = gamma*ex1
        c1 = 2.0/(gamma + 1)
        c2 = ((gamma - 1)/(gamma + 1))/aL
        c3 = 0.5*(gamma -1)
        d[ind] = dL*(c1 + c2*(uL - x[ind]/t))**ex1
        u[ind] = c1*(aL + c3*uL + x[ind]/t)
        p[ind] = pL*(c1 + c2*(uL - x[ind]/t))**ex2
        e[ind] = (p[ind]/d[ind])/(gamma - 1)
        # between the tail of the Left rarefaction and the contact disc
        ind = np.logical_and(x > xtL, x <= xc)
        d[ind] = dLL
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eLL
        # between the contact disc and the Right shock
        ind = np.logical_and(x > xc, x <= xhR)
        d[ind] = dRR
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eRR
        # to the right of the Right shock
        ind = np.where(x > xhR)
        d[ind] = dR
        u[ind] = uR
        p[ind] = pR
        e[ind] = (pR/dR)/(gamma - 1)
        # finally ...
        return x,d,u,p,e
        
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
    def __call__ (self, t):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, shR, stR = self.shL, self.shR, self.stR
        gamma = self.gamma
        # find the positions of the Left, contact and Right fronts
        xhL = shL*t
        xc  = ustar*t
        xtR = stR*t
        xhR = shR*t
        # define the x domain to include all the relevant fronts
        xL = xhL - 0.1*np.abs(xhR - xhL)
        xR = xhR + 0.1*np.abs(xhR - xhL)
        # define the variables in [xL,xR]
        # x,d,u,p,e = p1(t)
        x = np.linspace(xL,xR,201)
        d = np.zeros(len(x))
        u = np.zeros(len(x))
        p = np.zeros(len(x))
        e = np.zeros(len(x))
        # calculate d, u, p and e in the different regions
        # to the left of the Left shock
        ind = np.where(x <= xhL)
        d[ind] = dL
        u[ind] = uL
        p[ind] = pL
        e[ind] = (pL/dL)/(gamma - 1)
        # between the Left shock and the contact disc
        ind = np.logical_and(x > xhL, x <= xc)
        d[ind] = dLL
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eLL
        # between the contact disc and the tail of the Right rarefaction
        ind = np.logical_and(x > xc, x <= xtR)
        d[ind] = dRR
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eRR
        # inside the Right rarefaction
        ind = np.logical_and(x > xtR, x <= xhR)
        # Toro, eq 4.63, p135
        aR = np.sqrt(gamma*pR/dR)
        ex1 = 2.0/(gamma - 1)
        ex2 = gamma*ex1
        c1 = 2.0/(gamma + 1)
        c2 = ((gamma - 1)/(gamma + 1))/aR
        c3 = 0.5*(gamma -1)
        d[ind] = dR*(c1 - c2*(uR - x[ind]/t))**ex1
        u[ind] = c1*(-aR + c3*uR + x[ind]/t)
        p[ind] = pR*(c1 - c2*(uR - x[ind]/t))**ex2
        e[ind] = (p[ind]/d[ind])/(gamma - 1)
        # to the right of the Right rarefaction
        ind = np.where(x > xhR)
        d[ind] = dR
        u[ind] = uR
        p[ind] = pR
        e[ind] = (pR/dR)/(gamma - 1)
        # finally ...
        return x,d,u,p,e
        
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
    def __call__ (self, t):
        dL, uL, pL = self.dL, self.uL, self.pL
        dR,uR,pR = self.dR, self.uR, self.pR
        pstar,ustar = self.pstar, self.ustar
        dLL, dRR, eLL, eRR = self.dLL, self.dRR, self.eLL, self.eRR
        shL, shR = self.shL, self.shR
        gamma = self.gamma
        # find the positions of the Left, contact and Right fronts
        xhL = shL*t
        xc  = ustar*t
        xhR = shR*t
        # define the x domain to include all the relevant fronts
        xL = xhL - 0.1*np.abs(xhR - xhL)
        xR = xhR + 0.1*np.abs(xhR - xhL)
        # define the variables in [xL,xR]
        # x,d,u,p,e = p1(t)
        x = np.linspace(xL,xR,201)
        d = np.zeros(len(x))
        u = np.zeros(len(x))
        p = np.zeros(len(x))
        e = np.zeros(len(x))
        # calculate d, u, p and e in the different regions
        # to the left of the Left shock
        ind = np.where(x <= xhL)
        d[ind] = dL
        u[ind] = uL
        p[ind] = pL
        e[ind] = (pL/dL)/(gamma - 1)
        # between the Left shock and the contact disc
        ind = np.logical_and(x > xhL, x <= xc)
        d[ind] = dLL
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eLL
        # between the contact disc and the Right shock
        ind = np.logical_and(x > xc, x <= xhR)
        d[ind] = dRR
        u[ind] = ustar
        p[ind] = pstar
        e[ind] = eRR
        # to the right of the Right shock
        ind = np.where(x > xhR)
        d[ind] = dR
        u[ind] = uR
        p[ind] = pR
        e[ind] = (pR/dR)/(gamma - 1)
        # finally ...
        return x,d,u,p,e

class EXACT_RS:
    """
    If Ispeed = 1, it will return also the speeds of the Left and Right
    front waves:
        shL: Left wave, speed of its head
        stL: Left wave, speed of its head
        shR: Right wave, speed of its head
        stR: Right wave, speed of its tail.
        If a wave is a shock, then speed of the tail = speed of the head
    If Ispeed = 0, it will only return the following:
        x: array of x-coordinates in [xL,xR]. xL and xR are such that the
           complete physical wave, with its Left and Right fronts is included
        d: density array at x
        u: normal component of speed array at x
        p: pressure array at x
        e: internal energy array at x        
    """
    def __init__(self,dL,uL,pL,dR,uR,pR,Ispeed=0):
        self.dL,self.uL,self.pL = dL,uL,pL
        self.dR,self.uR,self.pR = dR,uR,pR
        self.Ispeed = Ispeed
    def __call__(self,t):
        dL,uL,pL = self.dL,self.uL,self.pL
        dR,uR,pR = self.dR,self.uR,self.pR
        Ispeed = self.Ispeed

        v = Vacuum_Check(dL,uL,pL,dR,uR,pR)
        v()         
        g = Guess_Initial_Pressure(dL,uL,pL,dR,uR,pR)
        p0 = g.PVRS()
        # find the pressure in the star region iteratively
        fp = Find_Pressure(dL,uL,pL,dR,uR,pR,p0)  # function f(p) in Fig 4.6
        # find f(p) = 0, with the condition that p > 0
        aa = 0
        bb = 10.0*max(pL,pR)
        pstar = brentq(fp,aa,bb)
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
            x,d,u,p,e = p1(t)  # calculate W(x,t)
        if pstar <= pL and pstar > pR:
            p2 = P2(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,stL,shR)
            x,d,u,p,e = p2(t)  # calculate W(x,t) 
        if pstar > pL and pstar <= pR:
            p3 = P3(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR,stR)
            x,d,u,p,e = p3(t)  # calculate W(x,t) 
        if pstar > pL and pstar > pR:
            p4 = P4(dL,uL,pL,dR,uR,pR,pstar,ustar,dLL,dRR,eLL,eRR,shL,shR)
            x,d,u,p,e = p4(t)  # calculate W(x,t)
        if Ispeed == 0:
            return x,d,u,p,e
        if Ispeed == 1: # returns also the speeds of the Left and Right waves
            return x,d,u,p,e,shL,stL,shR,stR

class Graphics:
    """
    generates the plots
    """
    def __init__(self,i,t,x,d,u,p,e):
        self.i, self.t = i,t
        self.x, self.d, self.u, self.p, self.e = x,d,u,p,e
    def __call__(self):
        i, t = self.i, self.t
        x,d,u,p,e = self.x, self.d, self.u, self.p, self.e
        plt.figure()
        plt.subplot(2,2,1)
        plt.plot(x,d)
        plt.grid()
        plt.title('Test # %d at = %g' % (i,t))
        plt.ylabel('density')
        plt.subplot(2,2,2)
        plt.plot(x,u)
        plt.grid()
        plt.title('EXACT RS')
        plt.ylabel('velocity')
        plt.subplot(2,2,3)
        plt.plot(x,p)
        plt.grid()
        plt.ylabel('pressure')
        plt.xlabel('position')
        plt.subplot(2,2,4)
        plt.plot(x,e)
        plt.grid()
        plt.ylabel('internal energy')
        plt.xlabel('position')
        plt.show()
        return 0

class Graphics01:
    """
    The simulation time in Toro's tests has been adjusted so that the
    solution is in [0,1]. One can extend the plots to [0,1] calling this
    program
    """
    def __init__(self,i,t,x,d,u,p,e):
        self.i, self.t = i,t
        self.x, self.d, self.u, self.p, self.e = x,d,u,p,e
    def __call__(self):
        i, t = self.i, self.t
        x,d,u,p,e = self.x, self.d, self.u, self.p, self.e

        xmin = min(x)
        xmax = max(x)

        if xmin > 0 and xmax < 1:
            xbelow = np.linspace(0,xmin)
            dbelow= d[0]*np.ones(len(xbelow))
            ubelow= u[0]*np.ones(len(xbelow))
            pbelow= p[0]*np.ones(len(xbelow))
            ebelow= e[0]*np.ones(len(xbelow))
            xabove = np.linspace(xmax,1)
            dabove = d[-1]*np.ones(len(xabove))
            uabove = u[-1]*np.ones(len(xabove))
            pabove = p[-1]*np.ones(len(xabove))
            eabove = e[-1]*np.ones(len(xabove))
            x01 = np.concatenate((xbelow,x,xabove))
            d01 = np.concatenate((dbelow,d,dabove))
            u01 = np.concatenate((ubelow,u,uabove))
            p01 = np.concatenate((pbelow,p,pabove))
            e01 = np.concatenate((ebelow,e,eabove))

            graphs = Graphics(i,t,x01,d01,u01,p01,e01)
            graphs()
            return x01,d01,u01,p01,e01
        else:
            print 'error: domain of original solution is not inside [0,1]'
            print 'Graphics01 cannot be extended to [0,1]'
            print 'xmin = %g' % (xmin)
            print 'xmax = %g' % (xmax)
            print 'Use Graphics instead'
            return -1

                
# ---------------------------------------------------------------
#                   MAIN PROGRAM
# ---------------------------------------------------------------
if __name__ == '__main__':

    plt.close('all')

    for i in range(1,6):
        if i == 1:  # Test 1: modified Sod test, Left rarefaction, R shock
            dL = 1.0
            uL = 0.75
            pL = 1.0
            dR = 0.125
            uR = 0.0
            pR = 0.1
        if i == 2:  # Test 2: Left rarefaction, R rarefaction
            dL = 1.0
            uL = -2.0
            pL = 0.4
            dR = 1.0
            uR = 2.0
            pR = 0.4   
        if i == 3:  # Test 3: Left rarefaction, R shock
            dL = 1.0
            uL = 0.0
            pL = 1000.0
            dR = 1.0
            uR = 0.0
            pR = 0.01   
        if i == 4:  # Test 4: Left shock, R rarefaction
            dL = 5.99924
            uL = 19.5975
            pL = 460.894
            dR = 5.99242
            uR = -6.19633
            pR = 46.0950
        if i == 5:  # Test 5: Left shock, R shock
            dL = 1.0
            uL = -19.59745
            pL = 1000.0
            dR = 1.0
            uR = -19.59745
            pR = 0.01

        # specify the time
        if i == 1:
            x0 = 0.3
            t = 0.2
        if i == 2:
            x0 = 0.5
            t = 0.15
        if i == 3:
            x0 = 0.5
            t = 0.012
        if i == 4:
            x0 = 0.4
            t = 0.035
        if i == 5:
            x0 = 0.8
            t = 0.012

        ex = EXACT_RS(dL,uL,pL,dR,uR,pR)
        x,d,u,p,e = ex(t)

        x = x + x0
        # plots
        if i == 3:
            graphs = Graphics(i,t,x,d,u,p,e)
            graphs()
        else:
            graphs = Graphics01(i,t,x,d,u,p,e)
            graphs()            
     



