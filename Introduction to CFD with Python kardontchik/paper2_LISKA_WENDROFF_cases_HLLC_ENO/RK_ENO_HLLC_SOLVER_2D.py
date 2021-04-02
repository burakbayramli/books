"""
    RK_ENO_HLLC_SOLVER_2D
   (we reconstruct the conserved variables, instead of d,u,p)
    with Runge-Kutta 3rd order, Shu, Lectures, Eq (4.11), p44
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from ENO_1D import *
from HLLC_FLUX import *

# ---------------------------------------------------------------
#               ENO_HLLC_SOLVER 2D
# ---------------------------------------------------------------

class ENO_HLLC_SOLVER_2D:
    """
    Solves the 1D Euler equations using Eq (9.2) and the
    HLLC_FLUX calculator. The Left and Right states for the
    HLLC solver use the reconstructed values obtained by ENO

    Iplot = n: plots intermediate results every n iterations
    """
    def __init__(self,CFL,x,y,dx,dy,p_ic,d_ic,u_ic,v_ic,interp=1,Iplot=999):
        self.CFL = CFL
        self.x,self.y = x,y
        self.dx, self.dy = dx,dy
        self.p_ic, self.d_ic, self.u_ic, self.v_ic = p_ic,d_ic,u_ic,v_ic
        self.interp = interp
        self.Iplot = Iplot

    def calc_eno_hllc_flux_2D(self,u1,u2,u3,u4):
        """
        # given the new u[i,j]'s, reconstruct the new u[i+1/2,j]'s and u[i,j+1/2] and
        # calculate the new fluxes
        """
        interp = self.interp

        xlen = int(u1.shape[0]) # length of u1 in the x-direction
        ylen = int(u1.shape[1]) # length of u1 in the y-direction

        xf1 = np.zeros((xlen,ylen))
        xf2 = np.zeros((xlen,ylen))
        xf3 = np.zeros((xlen,ylen))
        xf4 = np.zeros((xlen,ylen))
        yf1 = np.zeros((xlen,ylen))
        yf2 = np.zeros((xlen,ylen))
        yf3 = np.zeros((xlen,ylen))
        yf4 = np.zeros((xlen,ylen))

        xvflux = np.zeros(4)    # vectorial flux in the x-direction
        yvflux = np.zeros(4)
        
        gamma = 1.4
        if interp == 0:   # standard HLLC without reconstruction
            d = u1
            u = u2/u1
            v = u3/u1
            p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)  # Eq in p89
        if interp == 1:   # HLLC with ENO
            # reconstruct the conserved variables
            # in the x-direction
            xLu1 = np.zeros(np.shape(u1))
            xLu2 = np.zeros(np.shape(u2))
            xLu3 = np.zeros(np.shape(u3))
            xLu4 = np.zeros(np.shape(u4))
            xRu1 = np.zeros(np.shape(u1))
            xRu2 = np.zeros(np.shape(u2))
            xRu3 = np.zeros(np.shape(u3))
            xRu4 = np.zeros(np.shape(u4))
            # in the y-direction
            yLu1 = np.zeros(np.shape(u1))
            yLu2 = np.zeros(np.shape(u2))
            yLu3 = np.zeros(np.shape(u3))
            yLu4 = np.zeros(np.shape(u4))
            yRu1 = np.zeros(np.shape(u1))
            yRu2 = np.zeros(np.shape(u2))
            yRu3 = np.zeros(np.shape(u3))
            yRu4 = np.zeros(np.shape(u4))
            
            # (u1,u2,u3,u4): new cell averages at the end of Runge-Kutta's
            #                step I and step II
            # reconstruct values at interfaces (i+1/2,j) and (i,j+1/2)
            # reconstruct

            # ENO reconstruction in the x-direction
            for j in range(3,ylen-3):
                for i in range(3,xlen-3):
                    xrec = ENO_UL_UR(u1[i-2,j],u1[i-1,j],u1[i,j],u1[i+1,j],u1[i+2,j],u1[i+3,j])
                    xLu1[i,j],xRu1[i,j] = xrec()
                    xrec = ENO_UL_UR(u2[i-2,j],u2[i-1,j],u2[i,j],u2[i+1,j],u2[i+2,j],u2[i+3,j])
                    xLu2[i,j],xRu2[i,j] = xrec()
                    xrec = ENO_UL_UR(u3[i-2,j],u3[i-1,j],u3[i,j],u3[i+1,j],u3[i+2,j],u3[i+3,j])
                    xLu3[i,j],xRu3[i,j] = xrec()
                    xrec = ENO_UL_UR(u4[i-2,j],u4[i-1,j],u4[i,j],u4[i+1,j],u4[i+2,j],u4[i+3,j])
                    xLu4[i,j],xRu4[i,j] = xrec()
            # ENO reconstruction in the y-direction
            for i in range(3,xlen-3):
                for j in range(3,ylen-3):
                    yrec = ENO_UL_UR(u1[i,j-2],u1[i,j-1],u1[i,j],u1[i,j+1],u1[i,j+2],u1[i,j+3])
                    yLu1[i,j],yRu1[i,j] = yrec()
                    yrec = ENO_UL_UR(u2[i,j-2],u2[i,j-1],u2[i,j],u2[i,j+1],u2[i,j+2],u2[i,j+3])
                    yLu2[i,j],yRu2[i,j] = yrec()
                    yrec = ENO_UL_UR(u3[i,j-2],u3[i,j-1],u3[i,j],u3[i,j+1],u3[i,j+2],u3[i,j+3])
                    yLu3[i,j],yRu3[i,j] = yrec()
                    yrec = ENO_UL_UR(u4[i,j-2],u4[i,j-1],u4[i,j],u4[i,j+1],u4[i,j+2],u4[i,j+3])
                    yLu4[i,j],yRu4[i,j] = yrec()

            # ghost cells: transmissive boundary conditions
            # in the x-direction
            xLu1 = self.ghost_cells_xy(xLu1)
            xLu2 = self.ghost_cells_xy(xLu2)
            xLu3 = self.ghost_cells_xy(xLu3)
            xLu4 = self.ghost_cells_xy(xLu4)
            xRu1 = self.ghost_cells_xy(xRu1)
            xRu2 = self.ghost_cells_xy(xRu2)
            xRu3 = self.ghost_cells_xy(xRu3)
            xRu4 = self.ghost_cells_xy(xRu4)
            # in the y-direction
            yLu1 = self.ghost_cells_xy(yLu1)
            yLu2 = self.ghost_cells_xy(yLu2)
            yLu3 = self.ghost_cells_xy(yLu3)
            yLu4 = self.ghost_cells_xy(yLu4)
            yRu1 = self.ghost_cells_xy(yRu1)
            yRu2 = self.ghost_cells_xy(yRu2)
            yRu3 = self.ghost_cells_xy(yRu3)
            yRu4 = self.ghost_cells_xy(yRu4)

            # primitive variables at the interface between cells (x[i+1/2,y[j]] points
            # in the x-direction
            xLd = xLu1
            xLu = xLu2/xLu1
            xLv = xLu3/xLu1
            xLp = (gamma - 1.0)*(xLu4 - 0.5*(xLu2**2 + xLu3**2)/xLu1)  # Eq in p89
            xRd = xRu1
            xRu = xRu2/xRu1
            xRv = xRu3/xRu1
            xRp = (gamma - 1.0)*(xRu4 - 0.5*(xRu2**2 + xRu3**2)/xRu1)  # Eq in p89
            # in the y-direction
            yLd = yLu1
            yLu = yLu2/yLu1
            yLv = yLu3/yLu1
            yLp = (gamma - 1.0)*(yLu4 - 0.5*(yLu2**2 + yLu3**2)/yLu1)  # Eq in p89
            yRd = yRu1
            yRu = yRu2/yRu1
            yRv = yRu3/yRu1
            yRp = (gamma - 1.0)*(yRu4 - 0.5*(yRu2**2 + yRu3**2)/yRu1)  # Eq in p89

        # calculation of the fluxes in the x-direction
        for j in range(3,ylen-3):
            for i in range(2,xlen-3):
                if interp == 1:
                    TOL = 1.0e-6
                    dL = max(TOL,xLd[i,j]); dR = max(TOL,xRd[i,j])
                    uL = xLu[i,j]; uR = xRu[i,j]
                    vL = xLv[i,j]; vR = xRv[i,j]
                    pL = max(TOL,xLp[i,j]); pR = max(TOL,xRp[i,j])

                if interp == 0:
                    dL = d[i,j]
                    uL = u[i,j]
                    vL = v[i,j]
                    pL = p[i,j]
                    dR = d[i+1,j]
                    uR = u[i+1,j]
                    vR = v[i+1,j]
                    pR = p[i+1,j]

                # calculate the fluxes through the two edges
                if pL < 0 or pR < 0:
                    print 'negative pressure in ENO_HLLC_SOLVER_1D'
                    print 'pL = %g, pR = %g' % (pL,pR)
                if dL < 0 or dR < 0:
                    print 'negative density in ENO_HLLC_SOLVER_1D'
                    print 'dL = %g, dR = %g' % (dL,dR)
                flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                vflux,Sm = flu()
                xf1[i,j] = vflux[0]
                xf2[i,j] = vflux[1]
                xf3[i,j] = vflux[2]
                xf4[i,j] = vflux[3]

        # calculation of the fluxes in the y-direction
        for i in range(3,xlen-3):
            for j in range(2,ylen-3):
                if interp == 1:
                    TOL = 1.0e-6
                    dL = max(TOL,yLd[i,j]); dR = max(TOL,yRd[i,j])
                    uL = yLu[i,j]; uR = yRu[i,j]
                    vL = yLv[i,j]; vR = yRv[i,j]
                    pL = max(TOL,yLp[i,j]); pR = max(TOL,yRp[i,j])

                if interp == 0:
                    dL = d[i,j]
                    uL = u[i,j]
                    vL = v[i,j]
                    pL = p[i,j]
                    dR = d[i,j+1]
                    uR = u[i,j+1]
                    vR = v[i,j+1]
                    pR = p[i,j+1]

                # calculate the fluxes through the two edges
                if pL < 0 or pR < 0:
                    print 'negative pressure in ENO_HLLC_SOLVER_1D'
                    print 'pL = %g, pR = %g' % (pL,pR)
                if dL < 0 or dR < 0:
                    print 'negative density in ENO_HLLC_SOLVER_1D'
                    print 'dL = %g, dR = %g' % (dL,dR)
                #flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                # in the y-direction:
                # normal speed component is here v
                # tangential speed component is here u
                flu = HLLC_FLUX(dL,vL,uL,pL,dR,vR,uR,pR)
                vflux,Sm = flu()
                yf1[i,j] = vflux[0]
                yf2[i,j] = vflux[2]  # for u
                yf3[i,j] = vflux[1]  # for v
                yf4[i,j] = vflux[3]

        # return the corresponding new fluxes (no speeds are needed)
        return xf1,xf2,xf3,xf4,yf1,yf2,yf3,yf4

    def ghost_cells_xy(self,z):
        """
        z = 2D array in computational space (including the ghost cells)
        In a given calculation the sub-array in the physical domain,
                z[3:-3,3:-3]
        is calculated and these values are used to update the ghost cells

        The new array z, with the ghost cell values updated, is returned
        """
        # impose transmissive BC on the ghost cells
        # 3 ghost cells to the Left, 3 ghost cells to the Right
        # in the x-direction
        z[0,3:-3] =  z[1,3:-3] =  z[2,3:-3] =  z[3,3:-3]
        z[-1,3:-3] = z[-2,3:-3] = z[-3,3:-3] = z[-4,3:-3]
        # 3 ghost cells at the Bottom, 3 ghost cells at the Top
        # in the y-direction
        z[3:-3,0] =  z[3:-3,1] =  z[3:-3,2] =  z[3:-3,3]
        z[3:-3,-1] = z[3:-3,-2] = z[3:-3,-3] = z[3:-3,-4]
        # fill-in the small squares of ghost cells in the 4 corners
        z[0:3,0:3] = z[3,3]
        z[-3:,0:3] = z[-4,3]
        z[0:3,-3:] = z[3,-4]
        z[-3:,-3:] = z[-4,-4]
        return z
        
    def __call__(self,t):
        CFL = self.CFL
        x,y = self.x, self.y
        dx,dy = self.dx, self.dy      
        p_ic,d_ic,u_ic,v_ic = self.p_ic, self.d_ic, self.u_ic, self.v_ic
        interp = self.interp
        Iplot = self.Iplot

        gamma = 1.4
        # computational variables: physical space + ghost cells
        # 3 ghost cells on each side (Left, Right, Bottom, Top)
        xlen = int(p_ic.shape[0]) + 6
        ylen = int(p_ic.shape[1]) + 6
        p = np.zeros((xlen,ylen))
        d = np.zeros(np.shape(p))
        u = np.zeros(np.shape(p))
        v = np.zeros(np.shape(p))
        e = np.zeros(np.shape(p))
        a = np.zeros(np.shape(p))

        # initial conditions in physical space       
        p[3:-3,3:-3] = p_ic
        d[3:-3,3:-3] = d_ic
        u[3:-3,3:-3] = u_ic
        v[3:-3,3:-3] = v_ic

        # impose transmissive BC on the ghost cells
        p = self.ghost_cells_xy(p)
        d = self.ghost_cells_xy(d)
        u = self.ghost_cells_xy(u)
        v = self.ghost_cells_xy(v)
                
        # auxiliary variables
        e = (p/d)/(gamma - 1.0)   # internal energy, Eq 3.5, p88
        a = np.sqrt(gamma*p/d)    # speed of sound
        
        # convert to conserved variables, Eq 3.69, p104
        u1 = d
        u2 = d*u
        u3 = d*v
        u4 = d*(e + 0.5*(u**2 + v**2))   # Eq 3.66, p103

        # reconstruct the conserved variables
        # in the x-direction
        xLu1 = np.zeros(np.shape(u1))
        xLu2 = np.zeros(np.shape(u2))
        xLu3 = np.zeros(np.shape(u3))
        xLu4 = np.zeros(np.shape(u4))
        xRu1 = np.zeros(np.shape(u1))
        xRu2 = np.zeros(np.shape(u2))
        xRu3 = np.zeros(np.shape(u3))
        xRu4 = np.zeros(np.shape(u4))
        # in the y-direction
        yLu1 = np.zeros(np.shape(u1))
        yLu2 = np.zeros(np.shape(u2))
        yLu3 = np.zeros(np.shape(u3))
        yLu4 = np.zeros(np.shape(u4))
        yRu1 = np.zeros(np.shape(u1))
        yRu2 = np.zeros(np.shape(u2))
        yRu3 = np.zeros(np.shape(u3))
        yRu4 = np.zeros(np.shape(u4))
        
        # xLu1[i,j] = reconstructed value of u1 at (x[i+1/2],y[j]) coming from the Left
        # xRu1[i,j] = reconstructed value of u1 at (x[i+1/2],y[j]) coming from the Right
        # yLu1[i,j] = reconstructed value of u1 at (x[i],y[j+1/2]) coming from the Left
        # yRu1[i,j] = reconstructed value of u1 at (x[i],y[j+1/2]) coming from the Right
        # Note: for the y-direction, Left = Bottom, Right = Top
        
        # reconstruct
        # in the x-direction
        # xLu1[i,j] = reconstructed value of u1 at (x[i+1/2],y[j]_ coming from the Left,
        #   i.e., using (u[i-2,j],u[i-1,j],u[i,j]) or (u[i-1,j],u[i,j],u[i+1,j]) or
        #   (u[i,j],u[i+1,j],u[i+2,j]) (the smoothest combination)
        # xRu1[i] = reconstructed value of u1 at (x[i+1/2],y[j]) coming from the Right
        #   i.e., using (u[i+3,j],u[i+2,j],u[i+1,j]) or (u[i+2,j],u[i+1,j],u[i,j]) or
        #   (u[i+1,j],u[i,j],u[i-1,j]  (the smoothest combination)
        # If u[i,j] is continuous at (x[i+1/2],y[j]) then xLu[i,j] = xRu[i,j]
        # similar nomenclature is used for the y-direction


        # ENO reconstruction in the x-direction
        for j in range(3,ylen-3):
            for i in range(3,xlen-3):
                xrec = ENO_UL_UR(u1[i-2,j],u1[i-1,j],u1[i,j],u1[i+1,j],u1[i+2,j],u1[i+3,j])
                xLu1[i,j],xRu1[i,j] = xrec()
                xrec = ENO_UL_UR(u2[i-2,j],u2[i-1,j],u2[i,j],u2[i+1,j],u2[i+2,j],u2[i+3,j])
                xLu2[i,j],xRu2[i,j] = xrec()
                xrec = ENO_UL_UR(u3[i-2,j],u3[i-1,j],u3[i,j],u3[i+1,j],u3[i+2,j],u3[i+3,j])
                xLu3[i,j],xRu3[i,j] = xrec()
                xrec = ENO_UL_UR(u4[i-2,j],u4[i-1,j],u4[i,j],u4[i+1,j],u4[i+2,j],u4[i+3,j])
                xLu4[i,j],xRu4[i,j] = xrec()
        # ENO reconstruction in the y-direction
        for i in range(3,xlen-3):
            for j in range(3,ylen-3):
                yrec = ENO_UL_UR(u1[i,j-2],u1[i,j-1],u1[i,j],u1[i,j+1],u1[i,j+2],u1[i,j+3])
                yLu1[i,j],yRu1[i,j] = yrec()
                yrec = ENO_UL_UR(u2[i,j-2],u2[i,j-1],u2[i,j],u2[i,j+1],u2[i,j+2],u2[i,j+3])
                yLu2[i,j],yRu2[i,j] = yrec()
                yrec = ENO_UL_UR(u3[i,j-2],u3[i,j-1],u3[i,j],u3[i,j+1],u3[i,j+2],u3[i,j+3])
                yLu3[i,j],yRu3[i,j] = yrec()
                yrec = ENO_UL_UR(u4[i,j-2],u4[i,j-1],u4[i,j],u4[i,j+1],u4[i,j+2],u4[i,j+3])
                yLu4[i,j],yRu4[i,j] = yrec()

        # ghost cells: transmissive boundary conditions
        # in the x-direction
        xLu1 = self.ghost_cells_xy(xLu1)
        xLu2 = self.ghost_cells_xy(xLu2)
        xLu3 = self.ghost_cells_xy(xLu3)
        xLu4 = self.ghost_cells_xy(xLu4)
        xRu1 = self.ghost_cells_xy(xRu1)
        xRu2 = self.ghost_cells_xy(xRu2)
        xRu3 = self.ghost_cells_xy(xRu3)
        xRu4 = self.ghost_cells_xy(xRu4)
        # in the y-direction
        yLu1 = self.ghost_cells_xy(yLu1)
        yLu2 = self.ghost_cells_xy(yLu2)
        yLu3 = self.ghost_cells_xy(yLu3)
        yLu4 = self.ghost_cells_xy(yLu4)
        yRu1 = self.ghost_cells_xy(yRu1)
        yRu2 = self.ghost_cells_xy(yRu2)
        yRu3 = self.ghost_cells_xy(yRu3)
        yRu4 = self.ghost_cells_xy(yRu4)
        
        # primitive variables at the interface between cells (x[i+1/2,y[j]] points
        # in the x-direction
        xLd = xLu1
        xLu = xLu2/xLu1
        xLv = xLu3/xLu1
        xLp = (gamma - 1.0)*(xLu4 - 0.5*(xLu2**2 + xLu3**2)/xLu1)  # Eq in p89
        xRd = xRu1
        xRu = xRu2/xRu1
        xRv = xRu3/xRu1
        xRp = (gamma - 1.0)*(xRu4 - 0.5*(xRu2**2 + xRu3**2)/xRu1)  # Eq in p89
        # in the y-direction
        yLd = yLu1
        yLu = yLu2/yLu1
        yLv = yLu3/yLu1
        yLp = (gamma - 1.0)*(yLu4 - 0.5*(yLu2**2 + yLu3**2)/yLu1)  # Eq in p89
        yRd = yRu1
        yRu = yRu2/yRu1
        yRv = yRu3/yRu1
        yRp = (gamma - 1.0)*(yRu4 - 0.5*(yRu2**2 + yRu3**2)/yRu1)  # Eq in p89

       
        # fluxes:
        # xf[i,j] = flux crossing edge from cell [i,j] to cell [i+1,j]
        # xf[0,j] = flux crossing from 1st ghost cell at the Left end into
        #        the next cell to its right
        # xf[-1,j] = flux crossing from before last cell at the Right end and
        #        into the last ghost cell at the Right end
                # xf[i,j] = flux crossing edge from cell [i,j] to cell [i+1,j]

        # yf[i,j] = flux crossing edge from cell [i,j] to cell [i,j+1]                
        # yf[i,0] = flux crossing from 1st ghost cell at the Left (bottom) end into
        #        the next cell to its Right 
        # yf[i,-1] = flux crossing from before last cell at the Right end (top) and
        #        into the last ghost cell at the Right end
        
        xf1 = np.zeros((xlen,ylen))
        xf2 = np.zeros((xlen,ylen))
        xf3 = np.zeros((xlen,ylen))
        xf4 = np.zeros((xlen,ylen))
        yf1 = np.zeros((xlen,ylen))
        yf2 = np.zeros((xlen,ylen))
        yf3 = np.zeros((xlen,ylen))
        yf4 = np.zeros((xlen,ylen))

        xvflux = np.zeros(4)    # vectorial flux in the x-direction
        yvflux = np.zeros(4)

        # Runge-Kutta: initialization
        one_u1 = np.zeros((xlen,ylen))
        one_u2 = np.zeros((xlen,ylen))
        one_u3 = np.zeros((xlen,ylen))
        one_u4 = np.zeros((xlen,ylen))
        
        one_xf1 = np.zeros((xlen,ylen))
        one_xf2 = np.zeros((xlen,ylen))
        one_xf3 = np.zeros((xlen,ylen))
        one_xf4 = np.zeros((xlen,ylen))
        one_yf1 = np.zeros((xlen,ylen))
        one_yf2 = np.zeros((xlen,ylen))
        one_yf3 = np.zeros((xlen,ylen))
        one_yf4 = np.zeros((xlen,ylen))

        two_u1 = np.zeros((xlen,ylen))
        two_u2 = np.zeros((xlen,ylen))
        two_u3 = np.zeros((xlen,ylen))
        two_u4 = np.zeros((xlen,ylen))
        
        two_xf1 = np.zeros((xlen,ylen))
        two_xf2 = np.zeros((xlen,ylen))
        two_xf3 = np.zeros((xlen,ylen))
        two_xf4 = np.zeros((xlen,ylen))
        two_yf1 = np.zeros((xlen,ylen))
        two_yf2 = np.zeros((xlen,ylen))
        two_yf3 = np.zeros((xlen,ylen))
        two_yf4 = np.zeros((xlen,ylen))

        # initialize max speed per phys cell in the x- and y-directions
        xSmax_cell = np.zeros((xlen,ylen))
        ySmax_cell = np.zeros((xlen,ylen))
  
        # initial time step

        Sx = np.abs(u) + a          # Eq (16.38)
        Sy = np.abs(v) + a

        xSmax = np.max(Sx)
        ySmax = np.max(Sy)
        
        xdt = 0.2*dx/xSmax      # for initial iteration use CFL = 0.2    
        ydt = 0.2*dy/ySmax
        dt = min(xdt,ydt)       # Eqs 16.37 and 16.38

        tn = dt

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

            
            # calculation of the fluxes in the x-direction
            for j in range(3,ylen-3):
                for i in range(2,xlen-3):
                    if interp == 1:
                        TOL = 1.0e-6
                        dL = max(TOL,xLd[i,j]); dR = max(TOL,xRd[i,j])
                        uL = xLu[i,j]; uR = xRu[i,j]
                        vL = xLv[i,j]; vR = xRv[i,j]
                        pL = max(TOL,xLp[i,j]); pR = max(TOL,xRp[i,j])

                    if interp == 0:
                        dL = d[i,j]
                        uL = u[i,j]
                        vL = v[i,j]
                        pL = p[i,j]
                        dR = d[i+1,j]
                        uR = u[i+1,j]
                        vR = v[i+1,j]
                        pR = p[i+1,j]

                    # calculate the fluxes through the two edges
                    if pL < 0 or pR < 0:
                        print 'negative pressure in ENO_HLLC_SOLVER_1D'
                        print 'pL = %g, pR = %g' % (pL,pR)
                    if dL < 0 or dR < 0:
                        print 'negative density in ENO_HLLC_SOLVER_1D'
                        print 'dL = %g, dR = %g' % (dL,dR)
                    flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                    vflux,Sm = flu()
                    xf1[i,j] = vflux[0]
                    xf2[i,j] = vflux[1]
                    xf3[i,j] = vflux[2]
                    xf4[i,j] = vflux[3]
                    xSmax_cell[i,j] = Sm

            # calculation of the fluxes in the y-direction
            for i in range(3,xlen-3):
                for j in range(2,ylen-3):
                    if interp == 1:
                        TOL = 1.0e-6
                        dL = max(TOL,yLd[i,j]); dR = max(TOL,yRd[i,j])
                        uL = yLu[i,j]; uR = yRu[i,j]
                        vL = yLv[i,j]; vR = yRv[i,j]
                        pL = max(TOL,yLp[i,j]); pR = max(TOL,yRp[i,j])

                    if interp == 0:
                        dL = d[i,j]
                        uL = u[i,j]
                        vL = v[i,j]
                        pL = p[i,j]
                        dR = d[i,j+1]
                        uR = u[i,j+1]
                        vR = v[i,j+1]
                        pR = p[i,j+1]

                    # calculate the fluxes through the two edges
                    if pL < 0 or pR < 0:
                        print 'negative pressure in ENO_HLLC_SOLVER_1D'
                        print 'pL = %g, pR = %g' % (pL,pR)
                    if dL < 0 or dR < 0:
                        print 'negative density in ENO_HLLC_SOLVER_1D'
                        print 'dL = %g, dR = %g' % (dL,dR)
                    #flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                    # in the y-direction:
                    # normal speed component is here v
                    # tangential speed component is here u
                    flu = HLLC_FLUX(dL,vL,uL,pL,dR,vR,uR,pR)
                    vflux,Sm = flu()
                    yf1[i,j] = vflux[0]
                    yf2[i,j] = vflux[2]  # for u
                    yf3[i,j] = vflux[1]  # for v
                    yf4[i,j] = vflux[3]
                    ySmax_cell[i,j] = Sm
                   
            # -------------------------------------
            #           Runge-Kutta
            # -------------------------------------
            # Runge-Kutta
            # Nomenclature: L[u] = (1/dx)*(f[i-1/2) - f[i+1/2]
            
            # Step I:            
            # U[i] update, eq 9.2
            # remember (1D example, for simplicity, but the arrays are 2D):
            # u:  u[0]  u[1]  u[2]  u[3] ... u[i] ...  u[-4]  u[3]  u[-2]  u[-1]
            # f:  f[0]  f[1]  f[2]  f[3] ... f[i]      f[-4]  f[-3] f[-2]  f[-1]
            # u[3] increases due to f[2] that enters from the Left and
            # decreases due to f[3] that leaves to the Right
            # u[-3] increases due to f[-4] that enters from the Left and
            # decreases due to f[-3] that leaves to the Right
            # in 2D:
            # u[i,j] increases due to xf[i-1,j] and yf[i,j-1] entering from the Left and the Bottom,
            #        and decreases due to xf[i,j] and yf[i,j] leaving to the Right and to the Top

            one_u1[3:-3,3:-3] = u1[3:-3,3:-3] + (dt/dx)*(xf1[2:-4,3:-3] - xf1[3:-3,3:-3]) + (dt/dy)*(yf1[3:-3,2:-4] - yf1[3:-3,3:-3])
            one_u2[3:-3,3:-3] = u2[3:-3,3:-3] + (dt/dx)*(xf2[2:-4,3:-3] - xf2[3:-3,3:-3]) + (dt/dy)*(yf2[3:-3,2:-4] - yf2[3:-3,3:-3])
            one_u3[3:-3,3:-3] = u3[3:-3,3:-3] + (dt/dx)*(xf3[2:-4,3:-3] - xf3[3:-3,3:-3]) + (dt/dy)*(yf3[3:-3,2:-4] - yf3[3:-3,3:-3])
            one_u4[3:-3,3:-3] = u4[3:-3,3:-3] + (dt/dx)*(xf4[2:-4,3:-3] - xf4[3:-3,3:-3]) + (dt/dy)*(yf4[3:-3,2:-4] - yf4[3:-3,3:-3])
                             
            # update ghost cells: transmissive boundary conditions
            one_u1 = self.ghost_cells_xy(one_u1)
            one_u2 = self.ghost_cells_xy(one_u2)
            one_u3 = self.ghost_cells_xy(one_u3)
            one_u4 = self.ghost_cells_xy(one_u4)
            
            # Step II:
            # given the new u[i,j]'s, reconstruct the new u[i+1/2,j]'s and u[i,j+1/2] and
            # calculate the new fluxes
            one_xf1,one_xf2,one_xf3,one_xf4,one_yf1,one_yf2,one_yf3,one_yf4 =  \
                            self.calc_eno_hllc_flux_2D(one_u1,one_u2,one_u3,one_u4)

            two_u1[3:-3,3:-3] = 0.75*u1[3:-3,3:-3] + 0.25*one_u1[3:-3,3:-3] + \
                                0.25*(dt/dx)*(one_xf1[2:-4,3:-3] - one_xf1[3:-3,3:-3]) + \
                                0.25*(dt/dy)*(one_yf1[3:-3,2:-4] - one_yf1[3:-3,3:-3])
            two_u2[3:-3,3:-3] = 0.75*u2[3:-3,3:-3] + 0.25*one_u2[3:-3,3:-3] + \
                                0.25*(dt/dx)*(one_xf2[2:-4,3:-3] - one_xf2[3:-3,3:-3]) + \
                                0.25*(dt/dy)*(one_yf2[3:-3,2:-4] - one_yf2[3:-3,3:-3])
            two_u3[3:-3,3:-3] = 0.75*u3[3:-3,3:-3] + 0.25*one_u3[3:-3,3:-3] + \
                                0.25*(dt/dx)*(one_xf3[2:-4,3:-3] - one_xf3[3:-3,3:-3]) + \
                                0.25*(dt/dy)*(one_yf3[3:-3,2:-4] - one_yf3[3:-3,3:-3])
            two_u4[3:-3,3:-3] = 0.75*u4[3:-3,3:-3] + 0.25*one_u4[3:-3,3:-3] + \
                                0.25*(dt/dx)*(one_xf4[2:-4,3:-3] - one_xf4[3:-3,3:-3]) + \
                                0.25*(dt/dy)*(one_yf4[3:-3,2:-4] - one_yf4[3:-3,3:-3])


            # update ghost cells: transmissive boundary conditions
            two_u1 = self.ghost_cells_xy(two_u1)
            two_u2 = self.ghost_cells_xy(two_u2)
            two_u3 = self.ghost_cells_xy(two_u3)
            two_u4 = self.ghost_cells_xy(two_u4) 

            # Step III:
            # given the new u[i,j]'s, reconstruct the new u[i+1/2,j]'s and u[i,j+1/2] and
            # calculate the new fluxes
            two_xf1,two_xf2,two_xf3,two_xf4,two_yf1,two_yf2,two_yf3,two_yf4 =  \
                            self.calc_eno_hllc_flux_2D(two_u1,two_u2,two_u3,two_u4)

            c13 = 1.0/3.0; c23 = 2.0/3.0

            u1[3:-3,3:-3] = c13*u1[3:-3,3:-3] + c23*two_u1[3:-3,3:-3] + \
                                c23*(dt/dx)*(two_xf1[2:-4,3:-3] - two_xf1[3:-3,3:-3]) + \
                                c23*(dt/dy)*(two_yf1[3:-3,2:-4] - two_yf1[3:-3,3:-3])
            u2[3:-3,3:-3] = c13*u2[3:-3,3:-3] + c23*two_u2[3:-3,3:-3] + \
                                c23*(dt/dx)*(two_xf2[2:-4,3:-3] - two_xf2[3:-3,3:-3]) + \
                                c23*(dt/dy)*(two_yf2[3:-3,2:-4] - two_yf2[3:-3,3:-3])
            u3[3:-3,3:-3] = c13*u3[3:-3,3:-3] + c23*two_u3[3:-3,3:-3] + \
                                c23*(dt/dx)*(two_xf3[2:-4,3:-3] - two_xf3[3:-3,3:-3]) + \
                                c23*(dt/dy)*(two_yf3[3:-3,2:-4] - two_yf3[3:-3,3:-3])
            u4[3:-3,3:-3] = c13*u4[3:-3,3:-3] + c23*two_u4[3:-3,3:-3] + \
                                c23*(dt/dx)*(two_xf4[2:-4,3:-3] - two_xf4[3:-3,3:-3]) + \
                                c23*(dt/dy)*(two_yf4[3:-3,2:-4] - two_yf4[3:-3,3:-3])


            # update ghost cells: transmissive boundary conditions
            u1 = self.ghost_cells_xy(u1)
            u2 = self.ghost_cells_xy(u2)
            u3 = self.ghost_cells_xy(u3)
            u4 = self.ghost_cells_xy(u4)

            # ----------------------------------------
            #       END of Runge-Kutta
            # ----------------------------------------


            # update the primitive variables and the speed of sound
            # u,v, and a are needed to determine the next time step
            d = u1
            u = u2/u1
            v = u3/u1
            p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)  # Eq in p89
            a = np.sqrt(gamma*(p/d))  # not needed but gives a flag for error

            if interp == 1:
                # reconstruct
                # ENO reconstruction in the x-direction
                for j in range(3,ylen-3):
                    for i in range(3,xlen-3):
                        xrec = ENO_UL_UR(u1[i-2,j],u1[i-1,j],u1[i,j],u1[i+1,j],u1[i+2,j],u1[i+3,j])
                        xLu1[i,j],xRu1[i,j] = xrec()
                        xrec = ENO_UL_UR(u2[i-2,j],u2[i-1,j],u2[i,j],u2[i+1,j],u2[i+2,j],u2[i+3,j])
                        xLu2[i,j],xRu2[i,j] = xrec()
                        xrec = ENO_UL_UR(u3[i-2,j],u3[i-1,j],u3[i,j],u3[i+1,j],u3[i+2,j],u3[i+3,j])
                        xLu3[i,j],xRu3[i,j] = xrec()
                        xrec = ENO_UL_UR(u4[i-2,j],u4[i-1,j],u4[i,j],u4[i+1,j],u4[i+2,j],u4[i+3,j])
                        xLu4[i,j],xRu4[i,j] = xrec()
                # ENO reconstruction in the y-direction
                for i in range(3,xlen-3):
                    for j in range(3,ylen-3):
                        yrec = ENO_UL_UR(u1[i,j-2],u1[i,j-1],u1[i,j],u1[i,j+1],u1[i,j+2],u1[i,j+3])
                        yLu1[i,j],yRu1[i,j] = yrec()
                        yrec = ENO_UL_UR(u2[i,j-2],u2[i,j-1],u2[i,j],u2[i,j+1],u2[i,j+2],u2[i,j+3])
                        yLu2[i,j],yRu2[i,j] = yrec()
                        yrec = ENO_UL_UR(u3[i,j-2],u3[i,j-1],u3[i,j],u3[i,j+1],u3[i,j+2],u3[i,j+3])
                        yLu3[i,j],yRu3[i,j] = yrec()
                        yrec = ENO_UL_UR(u4[i,j-2],u4[i,j-1],u4[i,j],u4[i,j+1],u4[i,j+2],u4[i,j+3])
                        yLu4[i,j],yRu4[i,j] = yrec()

                # ghost cells: transmissive boundary conditions
                # in the x-direction
                xLu1 = self.ghost_cells_xy(xLu1)
                xLu2 = self.ghost_cells_xy(xLu2)
                xLu3 = self.ghost_cells_xy(xLu3)
                xLu4 = self.ghost_cells_xy(xLu4)
                xRu1 = self.ghost_cells_xy(xRu1)
                xRu2 = self.ghost_cells_xy(xRu2)
                xRu3 = self.ghost_cells_xy(xRu3)
                xRu4 = self.ghost_cells_xy(xRu4)
                # in the y-direction
                yLu1 = self.ghost_cells_xy(yLu1)
                yLu2 = self.ghost_cells_xy(yLu2)
                yLu3 = self.ghost_cells_xy(yLu3)
                yLu4 = self.ghost_cells_xy(yLu4)
                yRu1 = self.ghost_cells_xy(yRu1)
                yRu2 = self.ghost_cells_xy(yRu2)
                yRu3 = self.ghost_cells_xy(yRu3)
                yRu4 = self.ghost_cells_xy(yRu4)

                # primitive variables at the interface between cells (x[i+1/2,y[j]] points
                # in the x-direction
                xLd = xLu1
                xLu = xLu2/xLu1
                xLv = xLu3/xLu1
                xLp = (gamma - 1.0)*(xLu4 - 0.5*(xLu2**2 + xLu3**2)/xLu1)  # Eq in p89
                xRd = xRu1
                xRu = xRu2/xRu1
                xRv = xRu3/xRu1
                xRp = (gamma - 1.0)*(xRu4 - 0.5*(xRu2**2 + xRu3**2)/xRu1)  # Eq in p89
                # in the y-direction
                yLd = yLu1
                yLu = yLu2/yLu1
                yLv = yLu3/yLu1
                yLp = (gamma - 1.0)*(yLu4 - 0.5*(yLu2**2 + yLu3**2)/yLu1)  # Eq in p89
                yRd = yRu1
                yRu = yRu2/yRu1
                yRv = yRu3/yRu1
                yRp = (gamma - 1.0)*(yRu4 - 0.5*(yRu2**2 + yRu3**2)/yRu1)  # Eq in p89

            if iter % Iplot == 0:
                # calculate the primitive variables at time t:
                d = u1
                u = u2/u1
                v = u3/u1
                p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)   # p89 for 2D
                e = (p/d)/(gamma - 1.0)

                print ''
                print 'NEW PLOT AT ITERATION # %d' % (iter)
                print ''

                p = p.T
                d = d.T
                e = e.T
                u = u.T
                v = v.T
                
                plt.figure()
                plt.subplot(2,2,1)
                plt.imshow(p[3:-3,3:-3],cmap=plt.cm.jet,origin='lower')
                plt.colorbar()
                plt.title('pressure')
                plt.subplot(2,2,2)
                plt.imshow(d[3:-3,3:-3],cmap=plt.cm.jet,origin='lower')
                plt.colorbar()
                plt.title('density')
                plt.subplot(2,2,3)
                plt.imshow(e[3:-3,3:-3],cmap=plt.cm.jet,origin='lower')
                plt.colorbar()
                plt.ylabel('internal energy')
                plt.subplot(2,2,4)
                # (u,v) vector plot
                # generate 400 points for (u,v) plot
                if (xlen-6)*(ylen-6) <= 100 or (xlen-6) < 10 or (ylen-6) < 10:
                    plt.quiver(u[3:-3,3:-3],v[3:-3,3:-3])
                    ax = plt.subplot(2,2,4)
                    ax.yaxis.set_label_position('right')
                    plt.ylabel('velocity')
                    plt.axis('equal')
                    plt.show()
                else: # points in a given direction proportional to domain
                    k1 = 10.0/np.sqrt((x[-1] - x[0])*(y[-1] - y[0]))
                    Px = k1*(x[-1] - x[0])
                    Py = k1*(y[-1] - y[0])
                    Nx = int((xlen-6)/Px)
                    Ny = int((ylen-6)/Py)
                    plt.quiver(u[3:-3:Nx,3:-3:Ny],v[3:-3:Nx,3:-3:Ny])
                    ax = plt.subplot(2,2,4)
                    ax.yaxis.set_label_position('right')
                    plt.ylabel('velocity')
                    plt.axis('equal')
                    plt.xlabel('sim time = %g' % (tn))
                
                plt.show()

                p = p.T
                d = d.T
                e = e.T
                u = u.T
                v = v.T

            xSmax = np.max(np.abs(xSmax_cell))
            ySmax = np.max(np.abs(ySmax_cell))
            
            if tn < 0.1*t:
                xdt = 0.2*dx/xSmax      # use a smaller CFL number
                ydt = 0.2*dy/ySmax
                dt = min(xdt,ydt)       # Eqs 16.37 and 16.38
            else:
                xdt = CFL*dx/xSmax      
                ydt = CFL*dy/ySmax
                dt = min(xdt,ydt)       # Eqs 16.37 and 16.38

            print 'next dt = %g' % (dt)
            tn += dt           

        # calculate the values of d,u,v,p,e at time t (in physical space + ghosts)
        d = u1
        u = u2/u1
        v = u3/u1
        p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)   # p89 for 2D
        e = (p/d)/(gamma - 1.0)            
        return d[3:-3,3:-3],u[3:-3,3:-3],v[3:-3,3:-3],p[3:-3,3:-3],e[3:-3,3:-3]             
