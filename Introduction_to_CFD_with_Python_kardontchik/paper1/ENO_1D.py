"""
   ENO_1D
   Calculates the Left and Right states to be used in a 1D Rieman Problem
   using ENO polynomials of order k = 3
   The returned values, vLeft and vRight may be one of the primitive
   variables in a Riemann Solver: [d,u,v,p]  (density, x- and y- components
   of fluid speed, pressure) or one of the conserved variables in a
   Rieman solver: [U[1],U[2],U[3],U[4]] (density,density*u, density*v,energy)

   Order of accuracy of the polynomial approximation is k = 3 (Eq (2.5))
   
   Reference: Chi-Wang Shu, Lecture Notes, ICASE, November 1997
"""
import numpy as np

class ENO_UL_UR:
    """
    Uses ENO
    Calculate the extrapolated v[i+1/2] from the Left, vLeft, and from
    the Right, vRight, using a stencil based on 5 cells:
       S_Reft = [v[i-2],v[i-1],v[i],v[i+1],v[i+2]]  (k=3, from the Left)
       S_Right = [v[i+3],v[i+2],v[i+1],v[i],v[i-1]] (k=3, from the Right)
       Notice that the reference cell from the Left is the i-th cell, and
       the reference cell from the Right is the (i+1)-th cell
    """
    def __init__(self,v0,v1,v2,v3,v4,v5):
        """
        Nomenclature. See Shu, Eq (2.11) for k = 3
        v0 = v[i-2]  #(r=2 for the Left stencil)
        v1 = v[i-1]   
        v2 = v[i]    # reference cell for the Left stencil
        v3 = v[i+1]  # reference cell for the Right stencil
        v4 = v[i+2]
        v5 = v[i+3]  # (r=2 for the Right stencil)
        """
        self.v0,self.v1,self.v2,self.v3,self.v4,self.v5 = v0,v1,v2,v3,v4,v5
        
    def __call__(self):
        v0,v1,v2,v3,v4,v5 = self.v0,self.v1,self.v2,self.v3,self.v4,self.v5

        vL = np.zeros(3)
        vR = np.zeros(3)
        
        # v[i+1/2] using k=3,r=0, Table 2.1
        vL[0] = (1.0/3.0)*v2 + (5.0/6.0)*v3 - (1.0/6.0)*v4
        # v[i+1/2] using k=3,r=1, Table 2.1
        vL[1] = -(1.0/6.0)*v1 + (5.0/6.0)*v2 + (1./3.0)*v3
        # v[i+1/2] using k=3,r=2, Table 2.1
        vL[2] = (1.0/3.0)*v0 - (7.0/6.0)*v1 + (11.0/6.0)*v2

        # polynomials from the Right: reference cell = (i+1) cell
        # v[i+1/2] using k=3,r=0, Table 2.1
        vR[0] = (1.0/3.0)*v3 + (5.0/6.0)*v2 - (1.0/6.0)*v1
        # v[i+1/2] using k=3,r=1, Table 2.1
        vR[1] = -(1.0/6.0)*v4 + (5.0/6.0)*v3 + (1./3.0)*v2
        # v[i+1/2] using k=3,r=2, Table 2.1
        vR[2] = (1.0/3.0)*v5 - (7.0/6.0)*v4 + (11.0/6.0)*v3

        # smoothness indicators, Eq (2.61) and (2.63)
        betaL = np.zeros(3)
        betaR = np.zeros(3)
        # reference cell for the Left = i
        betaL[0] = (13.0/12.0)*(v2 - 2.0*v3 + v4)**2 + \
                 (1.0/4.0)*(3.0*v2 - 4.0*v3 + v4)**2
        betaL[1] = (13.0/12.0)*(v1 - 2.0*v2 + v3)**2 + \
                 (1.0/4.0)*(v1 - v3)**2
        betaL[2] = (13.0/12.0)*(v0 - 2.0*v1 + v2)**2 + \
                 (1.0/4.0)*(v0 - 4.0*v1 + 3.0*v2)**2
        # reference cell for the Right = (i+1)
        betaR[0] = (13.0/12.0)*(v3 - 2.0*v2 + v1)**2 + \
                 (1.0/4.0)*(3.0*v3 - 4.0*v2 + v1)**2
        betaR[1] = (13.0/12.0)*(v4 - 2.0*v3 + v2)**2 + \
                 (1.0/4.0)*(v4 - v2)**2
        betaR[2] = (13.0/12.0)*(v5 - 2.0*v4 + v3)**2 + \
                 (1.0/4.0)*(v5 - 4.0*v4 + 3.0*v3)**2
        # find smoothest polynomial:
        Lind = np.argmin(betaL)
        Rind = np.argmin(betaR)
        vLeft = vL[Lind]
        vRight = vR[Rind]
        return vLeft, vRight
