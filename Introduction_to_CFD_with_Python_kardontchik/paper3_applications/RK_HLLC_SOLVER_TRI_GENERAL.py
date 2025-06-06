"""
   file: RK_HLLC_SOLVER_TRI_GENERAL
   (Adds 3rd order Runge-Kutta)
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from HLLC_FLUX import *
from VISUAL_TRI_GENERAL import *  # for non-rectangular domains

# ---------------------------------------------------------------
#               HLLC-2D SOLVER
# ---------------------------------------------------------------

class HLLC_TRI:
    """
    Solves the 2D Euler equations using Eq (16.128)
    """
    def __init__(self,CFL,p_ic,d_ic,u_ic,v_ic, \
                 area,e01,e12,e20,ang01,ang12,ang20, \
                 neighbors, matched_pairs,tri_nodes,tri, \
                 trans_matched_pairs,refle_matched_pairs,force_matched_pairs, \
                 refle_bcos,refle_bsin, \
                 BC_forced,x1,x2,y1,y2,Iplot=9999):
        self.CFL = CFL
        self.p_ic, self.d_ic, self.u_ic, self.v_ic = p_ic,d_ic,u_ic,v_ic
        self.area, self.e01,self.e12,self.e20 = area,e01,e12,e20
        self.ang01,self.ang12,self.ang20 = ang01,ang12,ang20
        self.neighbors, self.matched_pairs = neighbors, matched_pairs
        self.trans_matched_pairs = trans_matched_pairs
        self.refle_matched_pairs = refle_matched_pairs
        self.force_matched_pairs = force_matched_pairs
        self.refle_bcos, self.refle_bsin = refle_bcos,refle_bsin
        self.BC_forced = BC_forced
        # for plotting
        self.x1,self.x2,self.y1,self.y2 = x1,x2,y1,y2
        self.tri_nodes, self.tri = tri_nodes,tri
        self.Iplot = Iplot

    def calc_hllc_flux_tri(self,u1,u2,u3,u4):
        area = self.area
        ang01, ang12,ang20 = self.ang01, self.ang12, self.ang20
        neighbors = self.neighbors
        # given the new u calculate the new fluxes
        # shape(u) = (len(p),1)     # physical domain plus ghosts
        # shape(f) = (len(area),3)  # only physical domain
        
        # update the primitive variables
        gamma = 1.4   # ideal gas        
        d = u1
        u = u2/u1
        v = u3/u1
        p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)  # Eq in p89

        # fluxes:
        # Ex: f1[i] = [f0,f1,f2] means
        # f0 = flux crossing edge ed01 of cell i in the out direction
        # f1 = flux crossing edge ed12 of cell i in the out direction
        # f2 = flux crossing edge ed20 of cell i in the out direction
        xf1 = np.zeros((len(area),3))
        xf2 = np.zeros((len(area),3))
        xf3 = np.zeros((len(area),3))
        xf4 = np.zeros((len(area),3))

        vflux = np.zeros(4)
        for i in range(0,len(area)):  # update in physical space
            # triangle i in physical space
            # calculate the fluxes through the three edges
            k1 = neighbors[i,0]
            k2 = neighbors[i,1]
            k3 = neighbors[i,2]
            nn = 0
            for k in [k1,k2,k3]:
                # calculate HLLC fluxes
                if k == k1:
                    ang = ang01[i]
                if k == k2:
                    ang = ang12[i]
                if k == k3:
                    ang = ang20[i]
                cosa = np.cos(ang)
                sina = np.sin(ang)
                # find the Left state variables
                # find the normal and tangential velocity components
                ux = u2[i]/u1[i]; uy = u3[i]/u1[i]
                un = cosa*ux + sina*uy
                ut = - sina*ux + cosa*uy  
                # Left state variables
                dL = u1[i]
                uL = un
                vL = ut
                pL = (gamma-1.0)*(u4[i] - 0.5*(u2[i]**2 + u3[i]**2)/u1[i])
                # find the Right state variables
                # find the normal and tangential velocity components
                ux = u2[k]/u1[k]; uy = u3[k]/u1[k]
                un = cosa*ux + sina*uy
                ut = - sina*ux + cosa*uy
                # Right state variables
                dR = u1[k]
                uR = un
                vR = ut
                pR = (gamma-1.0)*(u4[k] - 0.5*(u2[k]**2 + u3[k]**2)/u1[k])
                # Known (dL,uL,vL,pL) and (dR,uR,vR,pR) calculate the fluxes
                # using p331-332, eqs 10.67 to 10.73
                if (dL < 0) | (pL < 0) | (dR < 0) | (pR < 0):
                    print 'i =  %d, k = %d' % (i,k)
                    print 'dL = %g, uL = %g, vL = %g, pL = %g' %(dL,uL,vL,pL)
                    print 'dR = %g, uR = %g, vR = %g, pR = %g' %(dR,uR,vR,pR)
                flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                vflux, Sm = flu()

                xf1[i,nn] = vflux[0]
                xf2[i,nn] = cosa*vflux[1] - sina*vflux[2]
                xf3[i,nn] = sina*vflux[1] + cosa*vflux[2]
                xf4[i,nn] = vflux[3]
                # the above includes the rotate back with Eq 16.123
                nn += 1
        return xf1,xf2,xf3,xf4

    def __call__(self,t):
        CFL = self.CFL
        p_ic,d_ic,u_ic,v_ic = self.p_ic, self.d_ic, self.u_ic, self.v_ic 
        area,e01,e12,e20 = self.area, self.e01,self.e12,self.e20
        ang01,ang12,ang20 = self.ang01,self.ang12,self.ang20
        neighbors, matched_pairs = self.neighbors, self.matched_pairs
        trans_matched_pairs = self.trans_matched_pairs
        refle_matched_pairs = self.refle_matched_pairs
        force_matched_pairs = self.force_matched_pairs
        refle_bcos,refle_bsin = self.refle_bcos, self.refle_bsin
        BC_forced = self.BC_forced
        # for plotting
        x1,x2,y1,y2 = self.x1,self.x2,self.y1,self.y2
        tri_nodes, tri = self.tri_nodes, self.tri
        Iplot = self.Iplot

        # output variables
        pp = np.zeros(len(area))
        dd = np.zeros(len(area))
        uu = np.zeros(len(area))
        vv = np.zeros(len(area))
        ee = np.zeros(len(area))
        # internal variables use the computational space (including ghosts)
        # expand to computational grid
        p = np.zeros(len(area) + len(matched_pairs))
        d = np.zeros(len(p))
        u = np.zeros(len(p))
        v = np.zeros(len(p))
        e = np.zeros(len(p))   # internal energy

        # initial conditions
        p[0:len(area)] = p_ic
        d[0:len(area)] = d_ic
        u[0:len(area)] = u_ic
        v[0:len(area)] = v_ic

        # impose the BC
        if len(trans_matched_pairs): # not empty
            p[trans_matched_pairs[:,0]] = p[trans_matched_pairs[:,1]]
            d[trans_matched_pairs[:,0]] = d[trans_matched_pairs[:,1]]
            u[trans_matched_pairs[:,0]] = u[trans_matched_pairs[:,1]]
            v[trans_matched_pairs[:,0]] = v[trans_matched_pairs[:,1]]
        if len(refle_matched_pairs): # not empty
            p[refle_matched_pairs[:,0]] = p[refle_matched_pairs[:,1]]
            d[refle_matched_pairs[:,0]] = d[refle_matched_pairs[:,1]]

            refle_c1 = refle_bsin**2 - refle_bcos**2
            refle_c2 = -2.0*refle_bsin*refle_bcos

            u[refle_matched_pairs[:,0]] = \
               refle_c1 * u[refle_matched_pairs[:,1]] + \
               refle_c2 * v[refle_matched_pairs[:,1]]
            
            v[refle_matched_pairs[:,0]] = \
               refle_c2 * u[refle_matched_pairs[:,1]] - \
               refle_c1 * v[refle_matched_pairs[:,1]]
            
        if len(force_matched_pairs): # not empty
            p[force_matched_pairs[:,0]] = BC_forced[0]
            d[force_matched_pairs[:,0]] = BC_forced[1]
            u[force_matched_pairs[:,0]] = BC_forced[2]
            v[force_matched_pairs[:,0]] = BC_forced[3]         

        # auxiliary variables
        gamma = 1.4   # ideal gas
        e = (p/d)/(gamma - 1.0)   # internal energy, Eq 3.5, p88
        a = np.sqrt(gamma*p/d)    # speed of sound
        # convert to conserved variables, Eq 3.69, p104
        u1 = np.copy(d)
        u2 = d*u
        u3 = d*v
        u4 = d*(e + 0.5*(u**2 + v**2))   # Eq 3.66, p103

        one_u1 = np.zeros(len(p))
        one_u2 = np.zeros(len(p))
        one_u3 = np.zeros(len(p))
        one_u4 = np.zeros(len(p))

        two_u1 = np.zeros(len(p))
        two_u2 = np.zeros(len(p))
        two_u3 = np.zeros(len(p))
        two_u4 = np.zeros(len(p))
       
        # fluxes:
        # Ex: f1[i] = [f0,f1,f2] means
        # f0 = flux crossing edge ed01 of cell i in the out direction
        # f1 = flux crossing edge ed12 of cell i in the out direction
        # f2 = flux crossing edge ed20 of cell i in the out direction
        f1 = np.zeros((len(area),3))
        f2 = np.zeros((len(area),3))
        f3 = np.zeros((len(area),3))
        f4 = np.zeros((len(area),3))

        one_f1 = np.zeros((len(area),3))
        one_f2 = np.zeros((len(area),3))
        one_f3 = np.zeros((len(area),3))
        one_f4 = np.zeros((len(area),3))

        two_f1 = np.zeros((len(area),3))
        two_f2 = np.zeros((len(area),3))
        two_f3 = np.zeros((len(area),3))
        two_f4 = np.zeros((len(area),3))
        
        vflux = np.zeros(4)
  
        # initial time step
        max_speed = max(np.sqrt(u**2 + v**2))
        max_sound = max(a)
        Smax = max_speed + max_sound
        Amin = min(area)
        dx = np.sqrt(Amin)
        dt = CFL*dx/Smax        # Eqs 6.17 and 6.20
        tn = dt
        # initialize max speed per phys cell
        Smax_cell = np.zeros(len(area))
        Smax_edge = np.zeros(3)

        iter = 0
        while (tn <= t):
            Smax = 0.0
            iter += 1
            print 'iteration # %g, time = %g' % (iter, tn)
            for i in range(0,len(area)):  # update in physical space
                # triangle i in physical space
                # calculate the fluxes through the three edges
                k1 = neighbors[i,0]
                k2 = neighbors[i,1]
                k3 = neighbors[i,2]
                nn = 0
                for k in [k1,k2,k3]:
                    # calculate HLLC fluxes
                    if k == k1:
                        ang = ang01[i]
                    if k == k2:
                        ang = ang12[i]
                    if k == k3:
                        ang = ang20[i]
                    cosa = np.cos(ang)
                    sina = np.sin(ang)
                    # find the Left state variables
                    # find the normal and tangential velocity components
                    ux = u2[i]/u1[i]; uy = u3[i]/u1[i]
                    un = cosa*ux + sina*uy
                    ut = - sina*ux + cosa*uy  
                    # Left state variables
                    dL = u1[i]
                    uL = un
                    vL = ut
                    pL = (gamma-1.0)*(u4[i] - 0.5*(u2[i]**2 + u3[i]**2)/u1[i])
                    # find the Right state variables
                    # find the normal and tangential velocity components
                    ux = u2[k]/u1[k]; uy = u3[k]/u1[k]
                    un = cosa*ux + sina*uy
                    ut = - sina*ux + cosa*uy
                    # Right state variables
                    dR = u1[k]
                    uR = un
                    vR = ut
                    pR = (gamma-1.0)*(u4[k] - 0.5*(u2[k]**2 + u3[k]**2)/u1[k])
                    # Known (dL,uL,vL,pL) and (dR,uR,vR,pR) calculate the fluxes
                    # using p331-332, eqs 10.67 to 10.73
                    if (dL < 0) | (pL < 0) | (dR < 0) | (pR < 0):
                        print 'i =  %d, k = %d' % (i,k)
                        print 'dL = %g, uL = %g, vL = %g, pL = %g' %(dL,uL,vL,pL)
                        print 'dR = %g, uR = %g, vR = %g, pR = %g' %(dR,uR,vR,pR)
                    flu = HLLC_FLUX(dL,uL,vL,pL,dR,uR,vR,pR)
                    vflux, Sm = flu()

                    f1[i,nn] = vflux[0]
                    f2[i,nn] = cosa*vflux[1] - sina*vflux[2]
                    f3[i,nn] = sina*vflux[1] + cosa*vflux[2]
                    f4[i,nn] = vflux[3]
                    # the above includes the rotate back with Eq 16.123
                    Smax_edge[nn] = Sm
                    nn += 1
                Smax_cell[i] = max(Smax_edge)

                # ------------------------------------------------
                #       Runge-Kutta 3rd order
                # ------------------------------------------------
                #
                #  u1 = u[n] + dt*L(u[n])
                #  u2 =(3/4)*u[n] + (1/4)*u1 + (1/4)*dt*L(u1)
                #  u[n+1] = (1/3)*u[n] + (2/3)*u2 + (2/3)*dt*L(u2)
                # where
                #       L[u] = (1/dx)*(f[i-1/2) - f[i+1/2]
                # (the last expression should be translated from the 1D
                # domain to the triangular domain, as usual)

                # ----------------------------------------------------
                #               STEP I
                # ----------------------------------------------------
                
                # U[i] update, eq 16.128
                one_u1[i] = u1[i] - (dt/area[i])*(e01[i]*f1[i,0]+e12[i]*f1[i,1]+e20[i]*f1[i,2])
                one_u2[i] = u2[i] - (dt/area[i])*(e01[i]*f2[i,0]+e12[i]*f2[i,1]+e20[i]*f2[i,2])
                one_u3[i] = u3[i] - (dt/area[i])*(e01[i]*f3[i,0]+e12[i]*f3[i,1]+e20[i]*f3[i,2])
                one_u4[i] = u4[i] - (dt/area[i])*(e01[i]*f4[i,0]+e12[i]*f4[i,1]+e20[i]*f4[i,2])  
            # -----   update ghost cells: general boundary conditions --------------------
            # impose the BC
            if len(trans_matched_pairs): # not empty
                one_u1[trans_matched_pairs[:,0]] = one_u1[trans_matched_pairs[:,1]]
                one_u2[trans_matched_pairs[:,0]] = one_u2[trans_matched_pairs[:,1]]
                one_u3[trans_matched_pairs[:,0]] = one_u3[trans_matched_pairs[:,1]]
                one_u4[trans_matched_pairs[:,0]] = one_u4[trans_matched_pairs[:,1]]
            if len(refle_matched_pairs): # not empty
                one_u1[refle_matched_pairs[:,0]] = one_u1[refle_matched_pairs[:,1]]
                one_u4[refle_matched_pairs[:,0]] = one_u4[refle_matched_pairs[:,1]]

                refle_c1 = refle_bsin**2 - refle_bcos**2
                refle_c2 = -2.0*refle_bsin*refle_bcos

                one_u2[refle_matched_pairs[:,0]] = \
                   refle_c1 * one_u2[refle_matched_pairs[:,1]] + \
                   refle_c2 * one_u3[refle_matched_pairs[:,1]]
                
                one_u3[refle_matched_pairs[:,0]] = \
                   refle_c2 * one_u2[refle_matched_pairs[:,1]] - \
                   refle_c1 * one_u3[refle_matched_pairs[:,1]]
                
            if len(force_matched_pairs): # not empty
                # primitive variables
                p[force_matched_pairs[:,0]] = BC_forced[0]
                d[force_matched_pairs[:,0]] = BC_forced[1]
                u[force_matched_pairs[:,0]] = BC_forced[2]
                v[force_matched_pairs[:,0]] = BC_forced[3]
                e[force_matched_pairs[:,0]] =  \
                    (p[force_matched_pairs[:,0]]/d[force_matched_pairs[:,0]])/(gamma - 1.0)
                # convert to conserved variables
                ppp = p[force_matched_pairs[:,0]]
                ddd = d[force_matched_pairs[:,0]]
                uuu = u[force_matched_pairs[:,0]]
                vvv = v[force_matched_pairs[:,0]]
                eee = e[force_matched_pairs[:,0]]
                one_u1[force_matched_pairs[:,0]] = ddd
                one_u2[force_matched_pairs[:,0]] = ddd*uuu
                one_u3[force_matched_pairs[:,0]] = ddd*vvv             
                one_u4[force_matched_pairs[:,0]] = ddd*(eee + 0.5*(uuu**2 + vvv**2))                               
            # -------- end of update of ghost cells -------------------------

            # --------------------------------------------------
            #       END OF STEP I
            # --------------------------------------------------

            # --------------------------------------------------
            #       STEP II
            # --------------------------------------------------
            # calculate the new fluxes
            one_f1,one_f2,one_f3,one_f4 = \
                    self.calc_hllc_flux_tri(one_u1,one_u2,one_u3,one_u4)

            # update the conservative variables in the physical domain
            ia = range(len(area))
            two_u1[ia] = 0.75*u1[ia] + 0.25*one_u1[ia]  \
                    - 0.25*(dt/area[ia])*(e01[ia]*one_f1[ia,0]+e12[ia]*one_f1[ia,1]+e20[ia]*one_f1[ia,2])
            two_u2[ia] = 0.75*u2[ia] + 0.25*one_u2[ia]  \
                    - 0.25*(dt/area[ia])*(e01[ia]*one_f2[ia,0]+e12[ia]*one_f2[ia,1]+e20[ia]*one_f2[ia,2])
            two_u3[ia] = 0.75*u3[ia] + 0.25*one_u3[ia]  \
                    - 0.25*(dt/area[ia])*(e01[ia]*one_f3[ia,0]+e12[ia]*one_f3[ia,1]+e20[ia]*one_f3[ia,2])
            two_u4[ia] = 0.75*u4[ia] + 0.25*one_u4[ia]  \
                    - 0.25*(dt/area[ia])*(e01[ia]*one_f4[ia,0]+e12[ia]*one_f4[ia,1]+e20[ia]*one_f4[ia,2])

            # update the ghost cells
            # -----   update ghost cells: general boundary conditions --------------------
            # impose the BC
            if len(trans_matched_pairs): # not empty
                two_u1[trans_matched_pairs[:,0]] = two_u1[trans_matched_pairs[:,1]]
                two_u2[trans_matched_pairs[:,0]] = two_u2[trans_matched_pairs[:,1]]
                two_u3[trans_matched_pairs[:,0]] = two_u3[trans_matched_pairs[:,1]]
                two_u4[trans_matched_pairs[:,0]] = two_u4[trans_matched_pairs[:,1]]
            if len(refle_matched_pairs): # not empty
                two_u1[refle_matched_pairs[:,0]] = two_u1[refle_matched_pairs[:,1]]
                two_u4[refle_matched_pairs[:,0]] = two_u4[refle_matched_pairs[:,1]]

                refle_c1 = refle_bsin**2 - refle_bcos**2
                refle_c2 = -2.0*refle_bsin*refle_bcos

                two_u2[refle_matched_pairs[:,0]] = \
                   refle_c1 * two_u2[refle_matched_pairs[:,1]] + \
                   refle_c2 * two_u3[refle_matched_pairs[:,1]]
                
                two_u3[refle_matched_pairs[:,0]] = \
                   refle_c2 * two_u2[refle_matched_pairs[:,1]] - \
                   refle_c1 * two_u3[refle_matched_pairs[:,1]]
                
            if len(force_matched_pairs): # not empty
                # primitive variables
                p[force_matched_pairs[:,0]] = BC_forced[0]
                d[force_matched_pairs[:,0]] = BC_forced[1]
                u[force_matched_pairs[:,0]] = BC_forced[2]
                v[force_matched_pairs[:,0]] = BC_forced[3]
                e[force_matched_pairs[:,0]] =  \
                    (p[force_matched_pairs[:,0]]/d[force_matched_pairs[:,0]])/(gamma - 1.0)
                # convert to conserved variables
                ppp = p[force_matched_pairs[:,0]]
                ddd = d[force_matched_pairs[:,0]]
                uuu = u[force_matched_pairs[:,0]]
                vvv = v[force_matched_pairs[:,0]]
                eee = e[force_matched_pairs[:,0]]
                two_u1[force_matched_pairs[:,0]] = ddd
                two_u2[force_matched_pairs[:,0]] = ddd*uuu
                two_u3[force_matched_pairs[:,0]] = ddd*vvv             
                two_u4[force_matched_pairs[:,0]] = ddd*(eee + 0.5*(uuu**2 + vvv**2))                               
            # -------- end of update of ghost cells -------------------------
            # ---------------------------------------------------------------
            #      END OF STEP II
            # ---------------------------------------------------------------

            # --------------------------------------------------
            #       STEP III
            # --------------------------------------------------
            # calculate the new fluxes
            two_f1,two_f2,two_f3,two_f4 = \
                    self.calc_hllc_flux_tri(two_u1,two_u2,two_u3,two_u4)

            # update the conservative variables in the physical domain
            c13 = 1.0/3.0; c23 = 2.0/3.0            
            ia = range(len(area))
            u1[ia] = c13*u1[ia] + c23*two_u1[ia] \
                    - c23*(dt/area[ia])*(e01[ia]*two_f1[ia,0]+e12[ia]*two_f1[ia,1]+e20[ia]*two_f1[ia,2])
            u2[ia] = c13*u2[ia] + c23*two_u2[ia]  \
                    - c23*(dt/area[ia])*(e01[ia]*two_f2[ia,0]+e12[ia]*two_f2[ia,1]+e20[ia]*two_f2[ia,2])
            u3[ia] = c13*u3[ia] + c23*two_u3[ia]  \
                    - c23*(dt/area[ia])*(e01[ia]*two_f3[ia,0]+e12[ia]*two_f3[ia,1]+e20[ia]*two_f3[ia,2])
            u4[ia] = c13*u4[ia] + c23*two_u4[ia]  \
                    - c23*(dt/area[ia])*(e01[ia]*two_f4[ia,0]+e12[ia]*two_f4[ia,1]+e20[ia]*two_f4[ia,2])

            # update the ghost cells
            # -----   update ghost cells: general boundary conditions --------------------
            # impose the BC
            if len(trans_matched_pairs): # not empty
                u1[trans_matched_pairs[:,0]] = u1[trans_matched_pairs[:,1]]
                u2[trans_matched_pairs[:,0]] = u2[trans_matched_pairs[:,1]]
                u3[trans_matched_pairs[:,0]] = u3[trans_matched_pairs[:,1]]
                u4[trans_matched_pairs[:,0]] = u4[trans_matched_pairs[:,1]]
            if len(refle_matched_pairs): # not empty
                u1[refle_matched_pairs[:,0]] = u1[refle_matched_pairs[:,1]]
                u4[refle_matched_pairs[:,0]] = u4[refle_matched_pairs[:,1]]

                refle_c1 = refle_bsin**2 - refle_bcos**2
                refle_c2 = -2.0*refle_bsin*refle_bcos

                u2[refle_matched_pairs[:,0]] = \
                   refle_c1 * u2[refle_matched_pairs[:,1]] + \
                   refle_c2 * u3[refle_matched_pairs[:,1]]
                
                u3[refle_matched_pairs[:,0]] = \
                   refle_c2 * u2[refle_matched_pairs[:,1]] - \
                   refle_c1 * u3[refle_matched_pairs[:,1]]
                
            if len(force_matched_pairs): # not empty
                # primitive variables
                p[force_matched_pairs[:,0]] = BC_forced[0]
                d[force_matched_pairs[:,0]] = BC_forced[1]
                u[force_matched_pairs[:,0]] = BC_forced[2]
                v[force_matched_pairs[:,0]] = BC_forced[3]
                e[force_matched_pairs[:,0]] =  \
                    (p[force_matched_pairs[:,0]]/d[force_matched_pairs[:,0]])/(gamma - 1.0)
                # convert to conserved variables
                ppp = p[force_matched_pairs[:,0]]
                ddd = d[force_matched_pairs[:,0]]
                uuu = u[force_matched_pairs[:,0]]
                vvv = v[force_matched_pairs[:,0]]
                eee = e[force_matched_pairs[:,0]]
                u1[force_matched_pairs[:,0]] = ddd
                u2[force_matched_pairs[:,0]] = ddd*uuu
                u3[force_matched_pairs[:,0]] = ddd*vvv             
                u4[force_matched_pairs[:,0]] = ddd*(eee + 0.5*(uuu**2 + vvv**2))                               
            # -------- end of update of ghost cells -------------------------
            # ---------------------------------------------------------------
            #      END OF STEP III
            # ---------------------------------------------------------------

            # update the primitive variables and the speed of sound
            d = u1
            u = u2/u1
            v = u3/u1
            p = (gamma - 1.0)*(u4 - 0.5*(u2**2 + u3**2)/u1)  # Eq in p89
            a = np.sqrt(gamma*(p/d))

            if iter % Iplot == 0:
                # update the primitive variables in physical space
                dd = d[0:len(area)]
                uu = u[0:len(area)]
                vv = v[0:len(area)]
                pp = p[0:len(area)]

                print ''
                print 'NEW PLOT AT ITERATION # %d' % (iter)
                print ''                                
                # plot
                vis = VISUAL_2D(x1,x2,y1,y2,tn,tri_nodes,tri[0:len(area)],pp,dd,uu,vv,Ibrief=1)
                vis() 
                # end of Iplot 

            Smax = max(abs(Smax_cell))
            Amin = min(area)
            dx = np.sqrt(Amin)
            dt = CFL*dx/Smax
            print 'next dt = %g ' % (dt)
            tn += dt
            

        # calculate the values of d,u,v,p,e at time t (in physical space)
        dd = u1[0:len(area)]
        uu = u2[0:len(area)]/u1[0:len(area)]
        vv = u3[0:len(area)]/u1[0:len(area)]
        pp = (gamma - 1.0)*(u4[0:len(area)] - \
                0.5*(u2[0:len(area)]**2 + u3[0:len(area)]**2)/u1[0:len(area)])
        ee = (p/d)/(gamma - 1.0)            
        return dd,uu,vv,pp,ee             
