"""
    DEF_BLUNT_NOSE
    This file defines the computational domain of the Blunt Nose problem
    John Anderson et al , 'On hypersonic blunt body flow fields 
    obtained with a time-dependent technique', NOLTR 68-129, 1968
    case: cubic cylinder: x = 0.427*y**3 - 1
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from DISTMESH import *

# ----------------------------------------------------------------
#                   Cubic Cylinder
# ----------------------------------------------------------------

def dist_to_minimize(t,p,a):
    x = a*(np.abs(t))**3 - 1.0
    y = t
    dist = (p[0] - x)**2 + (p[1] - y)**2
    return dist

class Cubic_Cylinder:
    def __init__(self,a):
        self.a = a
        self.t, self.verts = self.pick_points_on_shape()

    def pick_points_on_shape(self):
        """
            Pick N points, more or less equally spaced, on the shape, 
            (we will call them vertices, because it is equivalent to
            defining a polygon circumscribed on the shape). They
            will be used to quickly find which vertex is closest to a
            given point p in the grid. The selected vertex will serve 
            as an initial reasonable close guess for Numpy's distance
            minimizer fmin
        """
        a = self.a      
        N = 81  # number of vertices
        t = np.linspace(-4,4,N)
        verts = np.zeros((N,2))
        verts[:,0] = a*(np.abs(t))**3 - 1.0
        verts[:,1] = t
        return t, verts

    def inside_shape(self,p):
        """
        returns 1 if the point p is in the computational domain,
        returns 0 if the point p is outside the computational domain
        """
        a = self.a
        x_on_curve = a*(np.abs(p[:,1]))**3 - 1
        inside = 0.5 + 0.5*np.sign(x_on_curve - p[:,0])
        return inside

    def find_closest_vertex(self,point):
        """
            For a given point in the grid, finds the minimum distance of
            the point to the selected points on the shape and returns the
            parametric variable t0. This t0 will be the initial good guess
            for Numpy's fmin that will find the exact distance between the
            point and the shape
        """
        t, verts = self.t, self.verts        
        dist = np.zeros(len(t))
        for i in range(len(t)):
            dist[i] = (point[0] - verts[i,0])**2 + (point[1] - verts[i,1])**2
        ind = np.argmin(dist)
        t0 = t[ind]
        return t0           

    def __call__(self,p):
        a = self.a
        t, verts = self.t, self.verts
        dist = np.zeros(len(p))
        inside = self.inside_shape(p)
        for j in range(len(p)):
            t0 =  self.find_closest_vertex(p[j])  # initial guess to minimizer
            opt = fmin(dist_to_minimize,t0, \
                       args=(p[j],a),full_output=1,disp=0)
            # add full_output=1 so we can retrieve the min dist(squared)
            # (2nd argument of opt array, 1st argument is the optimum t)
            min_dist = np.sqrt(opt[1])
            dist[j] = min_dist*(1 - 2*inside[j])
        return dist

# ----------------------------------------------------------------
#           End of Cubic
# ----------------------------------------------------------------

# ----------------------------------------------------------------
#       DISTANCE FUNCTION fd FOR COMPUTATIONAL DOMAIN
# ----------------------------------------------------------------

# rectangle
x1 = -2.0; x2 = 2.0; y1 = 0; y2 = 2.4
fd1 = Rectangle(x1,x2,y1,y2)
# Cubic
a = 0.427
fd2 = Cubic_Cylinder(a)
# computational domain
fd = Intersect(fd1,fd2)

# ----------------------------------------------------------------
#           END OF BLUNT_NOSE file
# ----------------------------------------------------------------

