"""
    CREATE_IDEAL_MESH_EQUI creates same-size equilateral triangles. The size of
    the equilateral triangles is determined by the parameter h0 (essentially,
    the length of an edge)

"""
import numpy as np
import matplotlib.pylab as plt

from DISTMESH import *
from Meshing_Tools import *

class CREATE_IDEAL_MESH_EQUI:
    """
   Input:
       h0 = defines the granularity of the mesh (equivalent to [dx, dy] in
            cartesian meshes. Roughly h0 ~ sqrt(2)*dx to get a similar
            number of total cells
    Returns:
        p: the position of the vertices of all the triangles in the mesh
        tri: the triangles defined each by its 3 vertices
        bbars: the pairs of nodes correspoonding to each bar (or triangle
            edge) that lies at the boundary of the mesh
    """
    def __init__(self,x1,x2,y1,y2,h0):
        self.x1,self.x2,self.y1,self.y2 = x1,x2,y1,y2
        self.h0 = h0
    def __call__(self):
        x1,x2,y1,y2 = self.x1,self.x2,self.y1,self.y2
        h0 = self.h0
        # mesh in [x1,x2]x[y1,y2]
        # Define the region in (x,y) plane in which we will insert the mesh
        xmin = x1 - 5*h0
        xmax = x2 + 5*h0
        ymin = y1 - 5*h0
        ymax = y2 + 5*h0

        # distance functions
        fd = Rectangle(x1,x2,y1,y2)
        # size function
        fh = lambda p: np.ones(len(p))

        # define the initial number of points in the grid in both directions
        h0x = h0; h0y = h0*np.sqrt(3)/2  # to obtain equilateral triangles
        Nx = int(np.floor((xmax - xmin)/h0x))
        Ny = int(np.floor((ymax - ymin)/h0y))
        x = np.linspace(xmin,xmax,Nx)
        y = np.linspace(ymin,ymax,Ny)
        # create the grid in the (x,y) plane
        xx,yy = np.meshgrid(x,y)
        # shift the even rows: it will reshape the rectangular grid of points
        # into a triangular-like grid. 
        xx[1::2] = xx[1::2] + h0x/2.0   # shifts even rows by h0x/2
        # p: points of the grid
        p = np.zeros((np.size(xx),2))
        p[:,0] = np.reshape(xx,np.size(xx))
        p[:,1] = np.reshape(yy,np.size(yy))

        # generate the triangles
        tt = Delaunay(p) # instantiate a class
        t = tt.vertices

        # keep the triangles whose geometrical center is inside the shape
        geps = 0.001*h0
        pmid = (p[t[:,0]] + p[t[:,1]] + p[t[:,2]])/3.0
        t = t[np.where(fd(pmid) < -geps)]
        # orient all triangles to be ccw
        t = ccw_tri(p,t)

        bars = np.concatenate((t[:,[0,1]],t[:,[0,2]], t[:,[1,2]]))
        # delete repeated bars
        bars = unique_rows(np.sort(bars))
        # plot the mesh
        ktrimesh(p,bars)
        # find the boundary bars
        tri,bbars = find_boundary(p,t,fh)

        return p,tri,bbars
