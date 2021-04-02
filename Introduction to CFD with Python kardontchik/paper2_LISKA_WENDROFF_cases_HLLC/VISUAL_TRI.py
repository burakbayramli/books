"""
   VISUAL_TRI

   Use only for rectangular domains. For non-rectangular domains use
   instead VISUAL_TRI_GENERAL
   
   generates plots for pressure, density and velocity field
   inputs:
        tri: triangles in the physical space [x1,x2]x[y1,y2],
             tri[i] = [p1, p2, p3]
        p: the coordinates [x,y] of the triangles' vertices
            p[j] = [xj, yj]
        pp, dd, uu, vv: the pressure, density, x-component of speed
            and y-component of speed in each triangle
"""
import numpy as np
import matplotlib.pylab as plt
from scipy.interpolate import griddata

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

        # define the grid
        grid_x,grid_y = np.mgrid[x1:x2:100j,y1:y2:100j]
        # define the points and variable at the points
        # cell location (middle of the triangle)
        cell = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])
        # pressure in the cells
        pp_tri = pp[0:len(tri)]
        # density at the cells
        dd_tri = dd[0:len(tri)]
        # grid the data for brief
        grid_p = griddata(cell,pp_tri,(grid_x,grid_y),method='nearest')
        grid_d = griddata(cell,dd_tri,(grid_x,grid_y),method='nearest')

        if Ibrief == 1:
            plt.figure()
            plt.subplot(2,2,1)
            cs = plt.contourf(grid_x,grid_y,grid_p,256,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('pressure')
            plt.ylabel('y')
            plt.axis('equal')

            plt.subplot(2,2,2)
            cs = plt.contourf(grid_x,grid_y,grid_d,256,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('density')
            plt.xlabel('x')
            plt.axis('equal')

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
                plt.quiver(uu_tri,vv_tri)
                plt.axis('equal')
                plt.xlabel('velocity field')
                plt.show()
            else:
                mxx = np.zeros(100)
                myy = np.zeros(100)
                muu = np.zeros(100)
                mvv = np.zeros(100)
                n = 0
                delx = (x2 - x1)/10.0
                dely = (y2 - y1)/10.0  
                for i in range(10):
                    for j in range(10):
                        ind = (xx> x1 + delx*i)&(xx<= x1 + delx*(i+1)) \
                              &(yy>y1 + dely*j)&(yy<=y1 + dely*(j+1))
                        mxx[n] = np.mean(xx[ind])
                        myy[n] = np.mean(yy[ind])
                        muu[n] = np.mean(uu_tri[ind])
                        mvv[n] = np.mean(vv_tri[ind])
                        n += 1
                plt.quiver(mxx,myy,muu,mvv)
                plt.axis('equal')
                plt.xlabel('velocity field')

            ind = np.argmin(pp_tri)
            pmin = pp[ind]
            # cell location (middle of the triangle)
            pmin_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])
            
            ind = np.argmax(pp_tri)
            pmax = pp[ind]
            # cell location (middle of the triangle)
            pmax_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])

            ind = np.argmin(dd_tri)
            dmin = dd[ind]
            # cell location (middle of the triangle)
            dmin_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])
            
            ind = np.argmax(dd_tri)
            dmax = dd[ind]
            # cell location (middle of the triangle)
            dmax_cell = (1.0/3.0)*(p[tri[ind,0]] + p[tri[ind,1]] + p[tri[ind,2]])

            plt.text(1.4*x2,y1 + 0.8*(y2-y1),'max p = %g at (%+5.2f,%+5.2f)' % (pmax,pmax_cell[0],pmax_cell[1]))
            plt.text(1.4*x2,y1 + 0.6*(y2-y1),'min p = %g at (%+5.2f,%+5.2f)' % (pmin,pmin_cell[0],pmin_cell[1]))
            plt.text(1.4*x2,y1 + 0.4*(y2-y1),'max d = %g at (%+5.2f,%+5.2f)' % (dmax,dmax_cell[0],dmax_cell[1]))
            plt.text(1.4*x2,y1 + 0.2*(y2-y1),'min d = %g at (%+5.2f,%+5.2f)' % (dmin,dmin_cell[0],dmin_cell[1]))
            plt.text(1.4*x2,y1,'simulation time t = %6.3f' % (t))
            plt.show()
            
        else: # normal detailed separate plots
            # grid the data for final plot
            grid_p = griddata(cell,pp_tri,(grid_x,grid_y),method='cubic')
            grid_d = griddata(cell,dd_tri,(grid_x,grid_y),method='cubic')
                       
            
            # ------------------------------
            #       pressure
            # ------------------------------
            # method 1: almost continuous contour plot
            plt.figure()
            cs = plt.contourf(grid_x,grid_y,grid_p,256,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('PRESSURE')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()
            
            # method 2
            plt.figure()
            cs = plt.contourf(grid_x,grid_y,grid_p,30,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('PRESSURE')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()

            # method 3
            plt.figure()
            plt.contour(grid_x,grid_y,grid_p,30)
            plt.colorbar()
            plt.title('PRESSURE')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()

            # --------------------
            #    density
            # --------------------
            # method 1: almost continuous contour plot
            plt.figure()
            cs = plt.contourf(grid_x,grid_y,grid_d,256,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()
            
            # method 2
            plt.figure()
            cs = plt.contourf(grid_x,grid_y,grid_d,30,cmap=plt.cm.jet)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()

            # method 3
            plt.figure()
            plt.contour(grid_x,grid_y,grid_d,30)
            plt.colorbar()
            plt.title('DENSITY')
            plt.xlabel('x')
            plt.ylabel('y')
            plt.axis('equal')
            plt.show()
            
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
                plt.quiver(uu_tri,vv_tri)
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
                n = 0
                delx = (x2 - x1)/10.0
                dely = (y2 - y1)/10.0                 
                for i in range(10):
                    for j in range(10):
                        ind = (xx> x1 + delx*i)&(xx<= x1 + delx*(i+1)) \
                              &(yy>y1 + dely*j)&(yy<=y1 + dely*(j+1))
                        mxx[n] = np.mean(xx[ind])
                        myy[n] = np.mean(yy[ind])
                        muu[n] = np.mean(uu_tri[ind])
                        mvv[n] = np.mean(vv_tri[ind])
                        n += 1
                plt.figure()
                plt.quiver(mxx,myy,muu,mvv)
                plt.title('VELOCITY FIELD')
                plt.axis('equal')
                plt.xlabel('pixel # in x')
                plt.ylabel('pixel # in y')
                plt.show()
                
        return 0
                
