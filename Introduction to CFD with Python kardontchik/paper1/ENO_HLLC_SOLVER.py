"""
   ENO_HLLC_SOLVER
   (we reconstruct the conserved variables, instead of d,u,p)
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from ENO_1D import *
from HLLC_FLUX import *

# ---------------------------------------------------------------
#               ENO_HLLC_SOLVER 1D
# ---------------------------------------------------------------

class ENO_HLLC_SOLVER_1D:
    """
    Solves the 1D Euler equations using Eq (9.2) and the
    HLLC_FLUX calculator. The Left and Right states for the
    HLLC solver use the reconstructed values obtained by WENO/ENO
    """
    def __init__(self,CFL,dx,p_ic,d_ic,u_ic,v_ic,interp=1):
        self.CFL = CFL
        self.dx = dx
        self.p_ic, self.d_ic, self.u_ic, self.v_ic = p_ic,d_ic,u_ic,v_ic
        self.interp = interp

    def __call__(self,t):
        CFL = self.CFL
        dx = self.dx       
        p_ic,d_ic,u_ic,v_ic = self.p_ic, self.d_ic, self.u_ic, self.v_ic
        interp = self.interp

        # output variables
        p = np.zeros(len(p_ic))
        d = np.zeros(len(p))
        u = np.zeros(len(p))
        v = np.zeros(len(p))
        e = np.zeros(len(p))

        # initial conditions        
        p[3:-3] = p_ic[3:-3]
        d[3:-3] = d_ic[3:-3]
        u[3:-3] = u_ic[3:-3]
        v[3:-3] = v_ic[3:-3]
        # impose transmissive BC on the ghost cells
        # 3 ghost cells to the Left, 3 ghost cells to the Right
        p[0] = p[1] = p[2] = p[3]
        d[0] = d[1] = d[2] = d[3]
        u[0] = u[1] = u[2] = u[3]
        v[0] = v[1] = v[2] = v[3]
        p[-1] = p[-2] = p[-3] = p[-4]
        d[-1] = d[-2] = d[-3] = d[-4]
        u[-1] = u[-2] = u[-3] = u[-4]
        v[-1] = v[-2] = v[-3] = v[-4]
        # auxiliary variables
        gamma = 1.4   # ideal gas
        e = (p/d)/(gamma - 1.0)   # internal energy, Eq 3.5, p88
        a = np.sqrt(gamma*p/d)    # speed of sound
        # convert to conserved variables, Eq 3.69, p104
        u1 = d
        u2 = d*u
        u3 = d*v
        u4 = d*(e + 0.5*(u**2 + v**2))   # Eq 3.66, p103

        # reconstruct the conserved variables
        Lu1 = np.zeros(len(u1))
        Lu2 = np.zeros(len(u2))
        Lu3 = np.zeros(len(u3))
        Lu4 = np.zeros(len(u4))
        Ru1 = np.zeros(len(u1))
        Ru2 = np.zeros(len(u2))
        Ru3 = np.zeros(len(u3))
        Ru4 = np.zeros(len(u4))
        # Lu1[i] = reconstructed value of u1 at x[i+1/2] coming from the Left
        # Ru1[i] = reconstructed value of u1 at x[i+1/2] coming from the Right
        # reconstruct
        # Lu1[i] = reconstructed value of u1 at x[i+1/2] coming from the Left,
        #   i.e., using (u[i-2],u[i-1],u[i]) or (u[i-1],u[i],u[i+1]) or
        #   (u[i],u[i+1],u[i+2]) (the smoothest combination)
        # Ru1[i] = reconstructed value of u1 at x[i+1/2] coming from the Right
        #   i.e., using (u[i+3],u[i+2],u[i+1]) or (u[i+2],u[i+1],u[i]) or
        #   (u[i+1],u[i],u[i-1]  (the smoothest combination)
        # If u[i] is continuous at x[i+1/2] then Lu[i] = Ru[i]
        for i in range(3,len(u1)-3):
            # we use ENO reconstruction
            rec = ENO_UL_UR(u1[i-2],u1[i-1],u1[i],u1[i+1],u1[i+2],u1[i+3])
            Lu1[i],Ru1[i] = rec()
            rec = ENO_UL_UR(u2[i-2],u2[i-1],u2[i],u2[i+1],u2[i+2],u2[i+3])
            Lu2[i],Ru2[i] = rec()
            rec = ENO_UL_UR(u3[i-2],u3[i-1],u3[i],u3[i+1],u3[i+2],u3[i+3])
            Lu3[i],Ru3[i] = rec()
            rec = ENO_UL_UR(u4[i-2],u4[i-1],u4[i],u4[i+1],u4[i+2],u4[i+3])
            Lu4[i],Ru4[i] = rec()
        # ghost cells: transmissive boundary conditions
        Lu1[0] = Lu1[1] = Lu1[2] = Lu1[3] 
        Lu2[0] = Lu2[1] = Lu2[2] = Lu2[3]
        Lu3[0] = Lu3[1] = Lu3[2] = Lu3[3] 
        Lu4[0] = Lu4[1] = Lu4[2] = Lu4[3]
        Lu1[-1] = Lu1[-2] = Lu1[-3] = Lu1[-4]
        Lu2[-1] = Lu2[-2] = Lu2[-3] = Lu2[-4]
        Lu3[-1] = Lu3[-2] = Lu3[-3] = Lu3[-4]
        Lu4[-1] = Lu4[-2] = Lu4[-3] = Lu4[-4]

        Ru1[0] = Ru1[1] = Ru1[2] = Ru1[3] 
        Ru2[0] = Ru2[1] = Ru2[2] = Ru2[3]
        Ru3[0] = Ru3[1] = Ru3[2] = Ru3[3] 
        Ru4[0] = Ru4[1] = Ru4[2] = Ru4[3]
        Ru1[-1] = Ru1[-2] = Ru1[-3] = Ru1[-4]
        Ru2[-1] = Ru2[-2] = Ru2[-3] = Ru2[-4]
        Ru3[-1] = Ru3[-2] = Ru3[-3] = Ru3[-4]
        Ru4[-1] = Ru4[-2] = Ru4[-3] = Ru4[-4]

        # primitive variables at the interface between cells (x[i+1/2] points)
        Ld = Lu1
        Lu = Lu2/Lu1
        Lv = Lu3/Lu1
        Lp = (gamma - 1.0)*(Lu4 - 0.5*(Lu2**2 + Lu3**2)/Lu1)  # Eq in p89
        Rd = Ru1
        Ru = Ru2/Ru1
        Rv = Ru3/Ru1
        Rp = (gamma - 1.0)*(Ru4 - 0.5*(Ru2**2 + Ru3**2)/Ru1)  # Eq in p89
       
        # fluxes:
        # f[i] = flux crossing edge from cell i to cell (i+1)
        # f[0] = flux crossing from 1st ghost cell at the Left end into
        #        the next cell to its right
        # f[-1] = flux crossing from before last cell at the Right and
        #        into the last ghost cell at the Right end
        f1 = np.zeros(len(p) - 1)
        f2 = np.zeros(len(f1))
        f3 = np.zeros(len(f1))
        f4 = np.zeros(len(f1))

        vflux = np.zeros(4)    # vectorial flux
  
        # initial time step
        max_speed = max(np.abs(u))
        max_sound = max(a)
        Smax = max_speed + max_sound
        dt = CFL*dx/Smax        # Eqs 6.17 and 6.20
        tn = dt
        if tn < 0.1*t:
            dt = 0.2*dx/Smax
        else:
            dt = CFL*dx/Smax    # Eqs 6.17 and 6.20
        # initialize max speed per phys cell (excluding the ghosts)
        Smax_cell = np.zeros(len(p) - 1)

        iter = 0
        while (tn <= t):
            Smax = 0.0
            iter += 1
            print 'iteration # %g, time = %g' % (iter, tn)
            # calculate the fluxes entering or leaving the physical cells
            # 1st physical cell is the i = 3 cell. (Cells i = 0,1,2 are
            # ghost cells). We must calculate the flux entering the i=3 
            # cell from the Left. That is, we must calculate vflux[2]
            # last physical cell is the i = -4 cell. (Cells i = -3,-2,-1
            # are ghost cells). We must calculate the flux leaving this
            # cell to the Right. That is, we must calculate the vflux[-4]
            
            for i in range(2,len(p)-3): # except in the ghost cells
                if interp == 1:
                    TOL = 1.0e-6
                    dL = max(TOL,Ld[i]); dR = max(TOL,Rd[i])
                    uL = Lu[i]; uR = Ru[i]
                    vL = Lv[i]; vR = Rv[i]
                    pL = max(TOL,Lp[i]); pR = max(TOL,Rp[i])

                if interp == 0:
                    dL = d[i]
                    uL = u[i]
                    vL = v[i]
                    pL = p[i]
                    dR = d[i+1]
                    uR = u[i+1]
                    vR = v[i+1]
                    pR = p[i+1]

                # calculate the fluxes through the two edges
                if pL < 0 or pR < 0:
                    print 'negative pressure in WENO_HLLC_SOLVER_1D'
                    print 'pL = %g, pR = %g' % (pL,pR)
                if dL < 0 or dR < 0:
                    print 'negative density in WENO_HLLC_SOLVER_1D'
                    print 'dL = %g, dR = %g' % (dL,dR)
                flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                vflux,Sm = flu()
                f1[i] = vflux[0]
                f2[i] = vflux[1]
                f3[i] = vflux[2]
                f4[i] = vflux[3]
                Smax_cell[i] = Sm

            # U[i] update, eq 9.2
            # remember:
            # u:  u[0]  u[1]  u[2]  u[3] ... u[i] ...  u[-4]  u[3]  u[-2]  u[-1]
            # f:  f[0]  f[1]  f[2]  f[3] ... f[i]      f[-3]  f[-2] f[-1]
            # the flux has one element less: len(f) = len(u) - 1
            # u[3] increases due to f[2] that enters from the Left and
            # decreases due to f[3] that leaves to the Right
            # u[-4] increases due to f[-4] that enters from the Left and
            # decreases due to f[-3] that leaves to the Right
            # Remember: f1[2:-3] means that the last included element is f1[-4]
            # and f1[3:-2] means that the last included element if f1[-3]
            u1[3:-3] = u1[3:-3] + (dt/dx)*(f1[2:-3] - f1[3:-2])
            u2[3:-3] = u2[3:-3] + (dt/dx)*(f2[2:-3] - f2[3:-2])
            u3[3:-3] = u3[3:-3] + (dt/dx)*(f3[2:-3] - f3[3:-2])
            u4[3:-3] = u4[3:-3] + (dt/dx)*(f4[2:-3] - f4[3:-2])                   
            # update ghost cells: transmissive boundary conditions
            u1[0] = u1[1] = u1[2] = u1[3]
            u2[0] = u2[1] = u2[2] = u2[3]
            u3[0] = u3[1] = u3[2] = u3[3]
            u4[0] = u4[1] = u4[2] = u4[3]
            u1[-1] = u1[-2] = u1[-3] = u1[-4]
            u2[-1] = u2[-2] = u2[-3] = u2[-4]
            u3[-1] = u3[-2] = u3[-3] = u3[-4]
            u4[-1] = u4[-2] = u4[-3] = u4[-4]

            if interp == 0:
            # update the primitive variables and the speed of sound
                d = u1
                u = u2/u1
                v = u3/u1
                p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)  # Eq in p89
                a = np.sqrt(gamma*(p/d))  # not needed but gives a flag for error

            if interp == 1:
                # reconstruct
                # Lu1[i] = reconstructed value of u1 at x[i+1/2] coming from the Left,
                #   i.e., using (u[i-2],u[i-1],u[i]) or (u[i-1],u[i],u[i+1]) or
                #   (u[i],u[i+1],u[i+2]) (the smoothest combination)
                # Ru1[i] = reconstructed value of u1 at x[i+1/2] coming from the Right
                #   i.e., using (u[i+3],u[i+2],u[i+1]) or (u[i+2],u[i+1],u[i]) or
                #   (u[i+1],u[i],u[i-1]  (the smoothest combination)
                # If u[i] is continuous at x[i+1/2] then Lu[i] = Ru[i]
                for i in range(2,len(p)-3): # except in the ghost cells
                    # we use ENO reconstruction
                    rec = ENO_UL_UR(u1[i-2],u1[i-1],u1[i],u1[i+1],u1[i+2],u1[i+3])
                    Lu1[i],Ru1[i] = rec()
                    rec = ENO_UL_UR(u2[i-2],u2[i-1],u2[i],u2[i+1],u2[i+2],u2[i+3])
                    Lu2[i],Ru2[i] = rec()
                    rec = ENO_UL_UR(u3[i-2],u3[i-1],u3[i],u3[i+1],u3[i+2],u3[i+3])
                    Lu3[i],Ru3[i] = rec()
                    rec = ENO_UL_UR(u4[i-2],u4[i-1],u4[i],u4[i+1],u4[i+2],u4[i+3])
                    Lu4[i],Ru4[i] = rec()
                # update the ghost cells: transmissive boundary conditions
                Lu1[0] = Lu1[1] = Lu1[2] = Lu1[3] 
                Lu2[0] = Lu2[1] = Lu2[2] = Lu2[3]
                Lu3[0] = Lu3[1] = Lu3[2] = Lu3[3] 
                Lu4[0] = Lu4[1] = Lu4[2] = Lu4[3]
                Lu1[-1] = Lu1[-2] = Lu1[-3] = Lu1[-4]
                Lu2[-1] = Lu2[-2] = Lu2[-3] = Lu2[-4]
                Lu3[-1] = Lu3[-2] = Lu3[-3] = Lu3[-4]
                Lu4[-1] = Lu4[-2] = Lu4[-3] = Lu4[-4]

                Ru1[0] = Ru1[1] = Ru1[2] = Ru1[3] 
                Ru2[0] = Ru2[1] = Ru2[2] = Ru2[3]
                Ru3[0] = Ru3[1] = Ru3[2] = Ru3[3] 
                Ru4[0] = Ru4[1] = Ru4[2] = Ru4[3]
                Ru1[-1] = Ru1[-2] = Ru1[-3] = Ru1[-4]
                Ru2[-1] = Ru2[-2] = Ru2[-3] = Ru2[-4]
                Ru3[-1] = Ru3[-2] = Ru3[-3] = Ru3[-4]
                Ru4[-1] = Ru4[-2] = Ru4[-3] = Ru4[-4]
                
                # update the L and R primitive variables and the speed of sound
                # at the x[i+1/2] interfaces
                Ld = Lu1
                Lu = Lu2/Lu1
                Lv = Lu3/Lu1
                Lp = (gamma - 1.0)*(Lu4 - 0.5*(Lu2**2 + Lu3**2)/Lu1)  # Eq in p89
                La = np.sqrt(gamma*(Lp/Ld))  # not needed but gives a flag for warning
                Rd = Ru1
                Ru = Ru2/Ru1
                Rv = Ru3/Ru1
                Rp = (gamma - 1.0)*(Ru4 - 0.5*(Ru2**2 + Ru3**2)/Ru1)  # Eq in p89
                Ra = np.sqrt(gamma*(Rp/Rd))  # not needed but gives a flag for warning

            Smax = max(abs(Smax_cell))
            if tn < 0.1*t:
                dt = 0.2*dx/Smax
            else:
                dt = CFL*dx/Smax
            print 'next dt = %g' % (dt)
            tn += dt

            

        # calculate the values of d,u,v,p,e at time t (in physical space + ghosts)
        d = u1
        u = u2/u1
        v = u3/u1
        p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)   # p89 for 2D
        e = (p/d)/(gamma - 1.0)            
        return d,u,v,p,e             
