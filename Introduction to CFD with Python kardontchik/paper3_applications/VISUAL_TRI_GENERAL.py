"""
   VISUAL_TRI_GENERAL

   The physical domain - that may have any shape - is inside a rectangular
   box [x1,x2]*[y1,y2]
   
   generates plots for pressure, density and velocity field
   inputs:
        tri: triangles in the physical space
             tri[i] = [p1, p2, p3]
        p: the coordinates [x,y] of the triangles' vertices
            p[j] = [xj, yj]
        pp, dd, uu, vv: the pressure, density, x-component of speed
            and y-component of speed in each triangle
"""
import numpy as np
import matplotlib.pylab as plt
from scipy.spatial import Delaunay

from DISTMESH import *

class VISUAL_2D:
    """
    If Ibrief = 1, puts all plots in one figure
    """
    def __init__(self,x1,x2,y1,y2,t,p,tri,pp,dd,uu,vv,Ibrief=0):
        self.x1,self.x2,self.y1,self.y2,self.t = x1,x2,y1,y2,t
        self.p, self.tri = p, tri
        self.pp, self.dd, self.uu, self.vv = pp, dd, uu, vv
        self.Ibrief = Ibrief
    def __call__(self):
        x1,x2,y1,y2,t = self.x1,self.x2,self.y1,self.y2,self.t
        p, tri = self.p, self.tri
        pp, dd, uu, vv = self.pp, self.dd, self.uu, self.vv
        Ibrief = self.Ibrief

        atm_units = 0   # False
        if np.max(pp) > 1.0e5:
            # change from Pascal units to 1 atm units
            atm_units = 1  # True
            p_1atm = 1.01325e5
            pp = pp/p_1atm

        # cell location (middle of the triangle)
        cell = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])

        # tricontour uses the pressure and density at the nodes, instead
        # of the calculated value of pp and dd in the cells, so we have
        # to assign a value to pressure and density to the nodes.

        # Sometimes, a few nodes do not appear in the triangulation
        # The pressure and density on these nodes will be defined as
        # the mean value of the pressure and density arrays. For normal
        # size meshes, this will not affect the visual plot
        ppp = np.zeros(len(p))
        ddd = np.zeros(len(p))
        pp_mean = np.mean(pp)
        dd_mean = np.mean(dd)
        for i in range(len(p)):
            ind0 = np.where(tri[:,0] == i)
            ind1 = np.where(tri[:,1] == i)
            ind2 = np.where(tri[:,2] == i)
            aa = list(np.concatenate((ind0[0],ind1[0],ind2[0])))
            if len(aa):  # not empty
                ppp[i] = np.mean(pp[aa])
                ddd[i] = np.mean(dd[aa])
            else:  # node not in tri
                ppp[i] = pp_mean
                ddd[i] = dd_mean
        
        if Ibrief == 1:
            
            plt.figure()
            plt.subplot(2,2,1)
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ppp,256)
            plt.colorbar()
            if atm_units == 0:
                plt.title('PRESSURE')
            else:
                plt.title('PRESSURE [ATM]')
            plt.ylabel('y')

            plt.subplot(2,2,2)
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ddd,256)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            
            plt.subplot(2,2,3)

            # --------------------------------
            #     u, v
            # --------------------------------
            xx = cell[:,0]
            yy = cell[:,1]
            uu_tri = uu[0:len(tri)]
            vv_tri = vv[0:len(tri)]

            # generate 100 points for (u,v) plot
            if len(xx)*len(yy) <= 100 or len(xx) < 10 or len(yy) < 10:
                plt.quiver(xx,yy,uu_tri,vv_tri)
                plt.axis('equal')
                plt.xlabel('velocity field')
                plt.show()
            else:
                mxx = np.zeros(100)
                myy = np.zeros(100)
                muu = np.zeros(100)
                mvv = np.zeros(100)

                ddx = 0.1*(x2 - x1);ddy = 0.1*(y2 - y1)
                n = 0
                
                for i in range(10):
                    for j in range(10):
                        ind = (xx>x1+ddx*i)&(xx<=x1+ddx*(i+1))&(yy>y1+ddy*j)&(yy<=y1+ddy*(j+1))
                        if np.any(ind == True):
                            mxx[n] = np.mean(xx[np.where(ind == True)])
                            myy[n] = np.mean(yy[np.where(ind == True)])
                            muu[n] = np.mean(uu_tri[np.where(ind == True)])
                            mvv[n] = np.mean(vv_tri[np.where(ind == True)])
                        else:
                            mxx[n] = x1 + 0.5*ddx*(2*i+1)
                            myy[n] = y1 + 0.5*ddy*(2*j+1)
                            muu[n] = 0.0
                            mvv[n] = 0.0
                        n += 1
                        
                plt.quiver(mxx,myy,muu,mvv)
                plt.axis('equal')
                plt.xlabel('velocity field')

            ind = np.argmin(pp)
            pmin = pp[ind]
            # cell location (middle of the triangle)
            pmin_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])
            
            ind = np.argmax(pp)
            pmax = pp[ind]
            # cell location (middle of the triangle)
            pmax_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])

            ind = np.argmin(dd)
            dmin = dd[ind]
            # cell location (middle of the triangle)
            dmin_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])
            
            ind = np.argmax(dd)
            dmax = dd[ind]
            # cell location (middle of the triangle)
            dmax_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])

            plt.text(1.2*x2,0.8*y2,'max p = %g @ (%+5.2f,%+5.2f)' % (pmax,pmax_cell[0],pmax_cell[1]))
            plt.text(1.2*x2,0.6*y2,'min p = %g @ (%+5.2f,%+5.2f)' % (pmin,pmin_cell[0],pmin_cell[1]))
            plt.text(1.2*x2,0.4*y2,'max d = %g @ (%+5.2f,%+5.2f)' % (dmax,dmax_cell[0],dmax_cell[1]))
            plt.text(1.2*x2,0.2*y2,'min d = %g @ (%+5.2f,%+5.2f)' % (dmin,dmin_cell[0],dmin_cell[1]))
            plt.text(1.2*x2,0.0,'simulation time t = %6.4f' % (t))
            plt.show()
            
        else: # normal detailed separate plots

            
            
            # ------------------------------
            #       pressure
            # ------------------------------
            # method 1: almost continuous contour plot
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ppp,256)
            plt.colorbar()
            if atm_units == 0:
                plt.title('PRESSURE')
            else:
                plt.title('PRESSURE [ATM]')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.show()

            # method 2: 30 contours
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ppp,30)
            plt.colorbar()
            if atm_units == 0:
                plt.title('PRESSURE')
            else:
                plt.title('PRESSURE [ATM]')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.show()

            # method 3: just the contour lines
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontour(p[:,0],p[:,1],tri,ppp,30)
            plt.colorbar()
            if atm_units == 0:
                plt.title('PRESSURE')
            else:
                plt.title('PRESSURE [ATM]')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.show()

            # --------------------
            #    density
            # --------------------
            # method 1: almost continuous contour plot
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ddd,256)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.show()

            # method 2: 30 contours
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontourf(p[:,0],p[:,1],tri,ddd,30)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.show()

            # method 3: just the contour lines
            plt.figure()
            plt.gca().set_aspect('equal')
            plt.tricontour(p[:,0],p[:,1],tri,ddd,30)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')         

            # --------------------------------
            #     u, v
            # --------------------------------
            xx = cell[:,0]
            yy = cell[:,1]
            uu_tri = uu[0:len(tri)]
            vv_tri = vv[0:len(tri)]
                
            # generate 100 points for (u,v) plot
            if len(xx)*len(yy) <= 100 or len(xx) < 10 or len(yy) < 10:
                plt.figure()
                plt.quiver(xx,yy,uu_tri,vv_tri)
                plt.title('VELOCITY FIELD')
                plt.axis('equal')
                plt.xlabel('pixel # in x')
                plt.ylabel('pixel # in y')
                plt.show()
            else:
                mxx = np.zeros(100)
                myy = np.zeros(100)
                muu = np.zeros(100)
                mvv = np.zeros(100)

                ddx = 0.1*(x2 - x1); ddy = 0.1*(y2 - y1)                
                n = 0
                
                for i in range(10):
                    for j in range(10):
                        ind = (xx>x1+ddx*i)&(xx<=x1+ddx*(i+1))&(yy>y1+ddy*j)&(yy<=y1+ddy*(j+1))
                        if np.any(ind == True):
                            mxx[n] = np.mean(xx[np.where(ind == True)])
                            myy[n] = np.mean(yy[np.where(ind == True)])
                            muu[n] = np.mean(uu_tri[np.where(ind == True)])
                            mvv[n] = np.mean(vv_tri[np.where(ind == True)])
                        else:
                            mxx[n] = x1 + 0.5*ddx*(2*i+1)
                            myy[n] = y1 + 0.5*ddy*(2*j+1)
                            muu[n] = 0.0
                            mvv[n] = 0.0
                        n += 1
                
                plt.figure()
                plt.quiver(mxx,myy,muu,mvv)
                plt.title('VELOCITY FIELD')
                plt.axis('equal')
                plt.xlabel('pixel # in x')
                plt.ylabel('pixel # in y')
                plt.show()
                
        return 0
                
