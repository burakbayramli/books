"""
   file: HLLC_SOLVER_TRI
"""
import numpy as np
from numpy import pi as pi
from scipy.optimize import brentq
import matplotlib.pylab as plt

from HLLC_FLUX import *
from VISUAL_TRI import *   # for rectangular domains

# ---------------------------------------------------------------
#               HLLC-2D SOLVER
# ---------------------------------------------------------------

class HLLC_TRI:
    """
    Solves the 2D Euler equations using Eq (16.128) 
    """
    def __init__(self,CFL,p_ic,d_ic,u_ic,v_ic, \
                 area,e01,e12,e20,ang01,ang12,ang20, \
                 neighbors, matched_pairs,tri_nodes,tri,x1=0,x2=1,y1=0,y2=1,Iplot=999):
        self.CFL = CFL
        self.p_ic, self.d_ic, self.u_ic, self.v_ic = p_ic,d_ic,u_ic,v_ic
        self.area, self.e01,self.e12,self.e20 = area,e01,e12,e20
        self.ang01,self.ang12,self.ang20 = ang01,ang12,ang20
        self.neighbors, self.matched_pairs = neighbors, matched_pairs
        # for plotting
        self.x1,self.x2,self.y1,self.y2 = x1,x2,y1,y2
        self.tri_nodes, self.tri = tri_nodes,tri
        self.Iplot = Iplot

    def __call__(self,t):
        CFL = self.CFL
        p_ic,d_ic,u_ic,v_ic = self.p_ic, self.d_ic, self.u_ic, self.v_ic 
        area,e01,e12,e20 = self.area, self.e01,self.e12,self.e20
        ang01,ang12,ang20 = self.ang01,self.ang12,self.ang20
        neighbors, matched_pairs = self.neighbors, self.matched_pairs
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
        # impose transmissive BC on the ghost triangles
        p[matched_pairs[:,0]] = p[matched_pairs[:,1]]
        d[matched_pairs[:,0]] = d[matched_pairs[:,1]]
        u[matched_pairs[:,0]] = u[matched_pairs[:,1]]
        v[matched_pairs[:,0]] = v[matched_pairs[:,1]]
        # auxiliary variables
        gamma = 1.4   # ideal gas
        e = (p/d)/(gamma - 1.0)   # internal energy, Eq 3.5, p88
        a = np.sqrt(gamma*p/d)    # speed of sound
        # convert to conserved variables, Eq 3.69, p104
        u1 = np.copy(d)
        u2 = d*u
        u3 = d*v
        u4 = d*(e + 0.5*(u**2 + v**2))   # Eq 3.66, p103
       
        # fluxes:
        # Ex: f1[i] = [f0,f1,f2] means
        # f0 = flux crossing edge ed01 of cell i in the out direction
        # f1 = flux crossing edge ed12 of cell i in the out direction
        # f2 = flux crossing edge ed20 of cell i in the out direction
        f1 = np.zeros((len(area),3))
        f2 = np.zeros((len(area),3))
        f3 = np.zeros((len(area),3))
        f4 = np.zeros((len(area),3))

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
                # U[i] update, eq 16.128
                u1[i] = u1[i] - (dt/area[i])*(e01[i]*f1[i,0]+e12[i]*f1[i,1]+e20[i]*f1[i,2])
                u2[i] = u2[i] - (dt/area[i])*(e01[i]*f2[i,0]+e12[i]*f2[i,1]+e20[i]*f2[i,2])
                u3[i] = u3[i] - (dt/area[i])*(e01[i]*f3[i,0]+e12[i]*f3[i,1]+e20[i]*f3[i,2])
                u4[i] = u4[i] - (dt/area[i])*(e01[i]*f4[i,0]+e12[i]*f4[i,1]+e20[i]*f4[i,2])  
            # update ghost cells: transmissive boundary conditions
            u1[matched_pairs[:,0]] = u1[matched_pairs[:,1]]
            u2[matched_pairs[:,0]] = u2[matched_pairs[:,1]]
            u3[matched_pairs[:,0]] = u3[matched_pairs[:,1]]
            u4[matched_pairs[:,0]] = u4[matched_pairs[:,1]]    

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
