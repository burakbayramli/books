"""
   VISUAL_2D_XY
   generates plots for pressure, density and velocity field
   in a cartesian grid
   inputs:
        coordinates (x,y) of the cells in the physical space [0,1]x[0,1],

        pp, dd, uu, vv: the pressure, density, x-component of speed
            and y-component of speed in each cell
"""
import numpy as np
import matplotlib.pylab as plt
from scipy.interpolate import griddata

class VISUAL_2D_XY:
    def __init__(self,x,y,pp,dd,uu,vv,levels = 100):
        self.x,self.y = x,y
        self.pp, self.dd, self.uu, self.vv = pp, dd, uu, vv
        self.levels = levels
    def __call__(self):
        x,y = self.x,self.y
        pp, dd, uu, vv = self.pp, self.dd, self.uu, self.vv
        levels = self.levels

        pp = pp.T    # transpose
        dd = dd.T
        uu = uu.T
        vv = vv.T
        
        # method 1
        # Note: if the number of pixels in the x- and y-direction is
        # very different you will get weird looking image plots
        plt.figure()
        plt.imshow(pp,cmap=plt.cm.jet,origin='lower')  # by color
        plt.colorbar()
        plt.title('PRESSURE')
        plt.xlabel('pixel # in x')
        plt.ylabel('pixel # in y')
        plt.show()

        # method 2
        plt.figure()
        plt.contourf(x,y,pp,29,cmap=plt.cm.jet)
        plt.colorbar()
        plt.axis('equal')
        plt.title('PRESSURE')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.show()

        # method 3
        plt.figure()
        plt.contour(x,y,pp,29)
        plt.colorbar()
        plt.axis('equal')
        plt.title('PRESSURE')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.show()

        # --------------------
        #    density
        # --------------------

        # method 1
        plt.figure()
        plt.imshow(dd,cmap=plt.cm.jet,origin='lower')  # by color
        plt.colorbar()
        plt.title('DENSITY')
        plt.xlabel('pixel # in x')
        plt.ylabel('pixel # in y')
        plt.show()

        # method 2
        plt.figure()
        plt.contourf(x,y,dd,29,cmap=plt.cm.jet)
        plt.colorbar()
        plt.axis('equal')
        plt.title('DENSITY')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.show()

        # method 3
        plt.figure()
        plt.contour(x,y,dd,29)
        plt.colorbar()
        plt.axis('equal')
        plt.title('DENSITY')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.show()
        
        # --------------------------------
        #     u, v
        # --------------------------------
        # generate 100 points for (u,v) plot
        if len(x)*len(y) <= 100 or len(x) < 10 or len(y) < 10:
            plt.figure()
            plt.quiver(uu,vv)
            plt.title('VELOCITY FIELD')
            plt.axis('equal')
            plt.xlabel('pixel # in x')
            plt.ylabel('pixel # in y')
            plt.show()
        else: # points in a given direction proportional to domain
            # warning: if dx != dy, visual shape != physical shape
            dx = (x[-1] - x[0])/len(x)
            dy = (y[-1] - y[-0])/len(y)
            if abs(dx - dy) > 0.001:
                print ''
                print 'Warning for plots using pixels:'
                print 'visual shape != physical shape because dx != dy'
                print 'Otherwise, it is OK'
                print ''
            k1 = 10.0/np.sqrt((x[-1] - x[0])*(y[-1] - y[0]))
            Px = k1*(x[-1] - x[0])
            Py = k1*(y[-1] - y[0])
            Nx = int(len(x)/Px)
            Ny = int(len(y)/Py)
            plt.figure()
            plt.quiver(uu[::Nx,::Ny],vv[::Nx,::Ny])
            plt.title('VELOCITY FIELD')
            plt.axis('equal')
            plt.xlabel('pixel # in x')
            plt.ylabel('pixel # in y')
            plt.show()

        pp = pp.T    # transpose
        dd = dd.T
        uu = uu.T
        vv = vv.T
        return 0
