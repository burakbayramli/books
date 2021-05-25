"""
   VISUAL_3D
   generates a 3D view for pressure or density
   inputs:
        tri: triangles in the physical space [0,1]x[0,1],
             tri[i] = [p1, p2, p3]
        p: the coordinates [x,y] of the triangles' vertices
            p[j] = [xj, yj]
        pp: the pressure or density
"""
import numpy as np
import matplotlib.pylab as plt
from scipy.interpolate import griddata
from mpl_toolkits.mplot3d import Axes3D

class VISUAL_3D:
    """
    set Ivar = 0 for pressure and Ivar = 1 for density
    """
    def __init__(self,x1,x2,y1,y2,p,tri):
        self.x1,self.x2,self.y1,self.y2 = x1,x2,y1,y2
        self.p, self.tri = p, tri

    def __call__(self,pp,Ivar):
        x1,x2,y1,y2 = self.x1,self.x2,self.y1,self.y2
        p, tri = self.p, self.tri

        # define the grid
        grid_x,grid_y = np.mgrid[x1:x2:100j,y1:y2:100j]
        # define the points and variable at the points
        # cell location (middle of the triangle)
        cell = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])
        # variable in the cells
        pp_tri = pp[0:len(tri)]

        grid_p = griddata(cell,pp_tri,(grid_x,grid_y),method='cubic')
            
        # replace any nan outside the physical domain by minimum value of variable
        grid_p[np.isnan(grid_p)] = np.min(pp_tri)
        
        plt.figure()
        ax = plt.gca(projection='3d')
        surf = ax.plot_surface(grid_x,grid_y,grid_p,rstride=1,cstride=1,cmap=plt.cm.Oranges)
        if Ivar == 0:
            plt.title('PRESSURE')
        if Ivar == 1:
            plt.title('DENSITY')
        plt.show()
            
        return 0
                
