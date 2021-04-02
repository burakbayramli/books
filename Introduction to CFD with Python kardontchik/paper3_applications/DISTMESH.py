
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
from scipy.optimize import fmin
import matplotlib.pylab as plt

from Meshing_Tools import *

"""
   DISTMESH was developed by Dr Per-Olof Persson, Department of Mathematics,
   Massachusetts Institute of Technology

   The original version of DISTMESH was written in MATLAB.

   This Python version of DISTMESH was developed by Dr Jaime Kardontchik,
   Sunnyvale, CA
  
"""

# ---------------------------------------------------------------
#                   DISTMESH - BASIC SHAPES
# ---------------------------------------------------------------

class Circle:
    """
    A circle is defined by its center (xc,yc) and its radius r
    """
    def __init__(self,xc,yc,r):
        self.xc, self.yc, self.r = xc, yc, r
    def __call__(self,p):
        xc, yc, r = self.xc, self.yc, self.r
        d = np.sqrt((p[:,0] - xc)**2 + (p[:,1] - yc)**2) - r
        return d

class Rectangle:
    """
    A rectangle is defined by its vertices (x1,y1) and (x2,y2)
    A point in the array p is inside the rectangle if
        x1 < p[:,0] < x2 and y1 < p[:,1] < y2
    It returns the minimum distance of point p to the edges of
    the rectangle
    """
    def __init__(self,x1,x2,y1,y2):
        self.x1, self.x2, self.y1, self.y2 = x1,x2,y1,y2
    def __call__(self,p):
        x1,x2,y1,y2 = self.x1, self.x2, self.y1, self.y2
        d1 = p[:,1] - y1    # if p inside d1 > 0
        d2 = y2 - p[:,1]    # if p inside d2 > 0
        d3 = p[:,0] - x1    # if p inside d3 > 0
        d4 = x2 - p[:,0]    # if p inside d4 > 0
        d =  -np.minimum(np.minimum(np.minimum(d1,d2),d3),d4)
        return d

class Polygon:
    """ the vertices of the polygon ('verts') is given as an array
        of orderly (counterclockwise) vertices along the polygon"
    """
    def __init__(self,verts):
        self.verts = verts
    def __call__(self,p):
        verts = self.verts
        # close the polygon
        cverts = np.zeros((len(verts)+1,2))
        cverts[0:-1] = verts
        cverts[-1] = verts[0]
        # initialize
        inside = np.zeros(len(p))
        dist = np.zeros(len(p))
        Cz = np.zeros(len(verts))  # z-components of the vectorial products
        dist_to_edge = np.zeros(len(verts))
        in_ref = np.ones(len(verts))
        # if np.sign(Cz) == in_ref then point is inside
        for j in range(len(p)):
            Cz = (cverts[1:,0] - cverts[0:-1,0])*(p[j,1] - cverts[0:-1,1]) - \
                 (cverts[1:,1] - cverts[0:-1,1])*(p[j,0] - cverts[0:-1,0])
            dist_to_edge = Cz/np.sqrt( \
                (cverts[1:,0] - cverts[0:-1,0])**2 + \
                (cverts[1:,1] - cverts[0:-1,1])**2)

            inside[j] = int(np.array_equal(np.sign(Cz),in_ref))
            dist[j] = (1 - 2*inside[j])*np.min(np.abs(dist_to_edge))
        return dist

class Union:
    def __init__(self,fd1,fd2):
        self.fd1, self.fd2 = fd1, fd2
    def __call__(self,p):
        fd1,fd2 = self.fd1, self.fd2
        d = np.minimum(fd1(p),fd2(p))
        return d

class Diff:
    def __init__(self,fd1,fd2):
        self.fd1, self.fd2 = fd1, fd2
    def __call__(self,p):
        fd1,fd2 = self.fd1, self.fd2
        d = np.maximum(fd1(p),-fd2(p))
        return d

class Intersect:
    def __init__(self,fd1,fd2):
        self.fd1, self.fd2 = fd1, fd2
    def __call__(self,p):
        fd1,fd2 = self.fd1, self.fd2
        d = np.maximum(fd1(p),fd2(p))
        return d

class Protate:
    """
    Rotates points p by angle phi counterclockwise around the origin
    """
    def __init__(self,phi):
        self.phi = phi
    def __call__(self,p):
        phi = self.phi
        c = np.cos(phi)
        s = np.sin(phi)
        temp = np.copy(p[:,0])
        rp = np.copy(p)
        rp[:,0] = c*p[:,0] - s*p[:,1]
        rp[:,1] = s*temp + c*p[:,1]
        return rp

class Protate_About:
    """
    Rotates points p by angle phi counterclockwise about a point pc
    """
    def __init__(self,phi,xc,yc):
        self.phi = phi
        self.xc, self.yc = xc, yc
    def __call__(self,p):
        phi = self.phi
        xc, yc = self.xc, self.yc
        pc = np.array([ [xc,yc] ])
        # Step I: change the origin of coordinates
        pp = p - pc
        # Step II: rotate pp about pc
        c = np.cos(phi)
        s = np.sin(phi)
        temp = np.copy(pp[:,0])
        rp = np.copy(pp)
        rp[:,0] = c*pp[:,0] - s*pp[:,1]
        rp[:,1] = s*temp + c*pp[:,1]
        # Step III: go back to the original frame
        rp = rp + pc
        return rp

class Pshift:
    """
    Shifts points p by a constant vector (x0,y0)
    """
    def __init__(self,x0,y0):
        self.x0, self.y0 = x0,y0
    def __call__(self,p):
        x0, y0 = self.x0, self.y0
        p[:,0] = p[:,0] + x0
        p[:,1] = p[:,1] + y0
        return p

# ------------------------------------------------------------
#               ELLIPSE
# ------------------------------------------------------------

def Ellipse_dist_to_minimize(t,p,xc,yc,a,b):
    """
       Given a point p in the grid and a point on the ellipse,
       defined by its parametric variable t, finds the distance
       between these two points.
       Since this distance will be used only by the minimizer,
       it is more efficient to avoid calculating the sqrt.
       Numpy's fmin will vary the variable t to minimize the
       distance between the point p in the grid and the Ellipse
       """
    x = xc + a*np.cos(t)    # coord x of the point on the ellipse
    y = yc + b*np.sin(t)    # coord y of the point on the ellipse
    dist = (p[0] - x)**2 + (p[1] - y)**2
    return dist

class Ellipse:
    def __init__(self,xc,yc,a,b):
        self.xc, self.yc, self.a, self.b = xc, yc, a, b
        self.t, self.verts = self.pick_points_on_shape()

    def pick_points_on_shape(self):
        """
            Pick N points, more or less equally spaced, on the ellipse, 
            (we will call them vertices, because it is equivalent to
            defining a polygon circumscribed on the ellipse). They
            will be used to quickly find which vertex is closest to a
            given point p in the grid. The selected vertex will serve 
            as an initial reasonable close guess for Numpy's distance
            minimizer fmin
        """
        xc, yc, a, b = self.xc, self.yc, self.a, self.b        
        c = np.array([xc,yc])
        t = np.linspace(0,(7.0/4.0)*pi,8)
        verts = np.zeros((8,2))
        verts[:,0] = c[0] + a*np.cos(t)
        verts[:,1] = c[1] + b*np.sin(t)
        return t, verts
        
    def inside_ellipse(self,p):
        """
        returns 1 if the point p is inside, returns 0 if outside
        """
        xc, yc, a, b = self.xc, self.yc, self.a, self.b
        c = np.array([xc,yc])
        r, phase = self.rect_to_polar(p-c)
        r_ellipse = self.rellipse(phase)
        in_ref = np.ones(len(p))
        inside = 0.5 + 0.5*np.sign(r_ellipse-r)
        return inside

    def rect_to_polar(self,p):
        """
        given a point p on the grid with cartesian coordinates (x,y),
        find its polar coordinates (r,phase)
        """
        r = np.sqrt(p[:,0]**2 + p[:,1]**2)
        phase = np.arctan2(p[:,1],p[:,0])
        # note: np.arctan2(y,x) order; phase in +/- pi (+/- 180deg)
        return r, phase

    def rellipse(self,phi):
        """
        Given the semi-axes of the ellipse (a,b), a > b, and the angle phi
        that a point on the ellipse makes with the semi-axis a ('x' axis)
        find the distance r of the point to the center of the ellipse
        """
        a, b = self.a, self.b
        r = a*b/np.sqrt((b*np.cos(phi))**2 + (a*np.sin(phi))**2)
        return r

    def find_closest_vertex(self,point):
        """
            For a given point in the grid, finds the minimum distance of
            the point to the selected points on the ellipse and returns the
            parametric variable t0 and the coordinates of the point on
            the ellipse. This t0 will be the initial good guess for Numpy's
            fmin that will find the exact distance between the point and the
            ellipse
            """
        t, verts = self.t, self.verts
        
        dist = np.zeros(len(t))
        for i in range(len(t)):
            dist[i] = (point[0] - verts[i,0])**2 + (point[1] - verts[i,1])**2
        ind = np.argmin(dist)
        t0 = t[ind]
        return t0   
    
    def __call__(self,p):
        xc, yc, a, b = self.xc, self.yc, self.a, self.b
        t, verts = self.t, self.verts
        dist = np.zeros(len(p))
        inside = self.inside_ellipse(p)
        for j in range(len(p)):
            t0 =  self.find_closest_vertex(p[j])  # initial guess to minimizer
            opt = fmin(Ellipse_dist_to_minimize,t0, \
                       args=(p[j],xc,yc,a,b),full_output=1,disp=0)
            # add full_output=1 so we can retrieve the min dist(squared)
            # (2nd argument of opt array, 1st argument is the optimum t)
            min_dist = np.sqrt(opt[1])
            dist[j] = min_dist*(1 - 2*inside[j])
        return dist

# --------------------------------------------------------------------
#                   DISTMESH - core
# --------------------------------------------------------------------

def distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,ttol=0.1,dptol=0.001,Iflag=1,qmin=1.0):
    #Iflag = 0: do not print internal sim status and
    #           do not plot intermediate meshes
    #Iflag = 1:(default) print internal simulation status only
    #Iflag = 2: plot intermediate meshes only
    #Iflag = 3: print internal sim status and plot intermediate meshes
    #Iflag = 4: prints the Delaunay iteration, minimum q and angle(deg)
    
    # Constants
    geps = 0.001*h0; deltat = 0.2; Fscale = 1.2
    deps = h0 * np.sqrt(np.spacing(1))

    random_seed = 17

    # define the initial number of points in the grid in both directions
    h0x = h0; h0y = h0*np.sqrt(3)/2  # to obtain equilateral triangles
    Nx = int(np.floor((xmax - xmin)/h0x))
    Ny = int(np.floor((ymax - ymin)/h0y))
    x = np.linspace(xmin,xmax,Nx)
    y = np.linspace(ymin,ymax,Ny)
    # create the grid in the (x,y) plane
    xx,yy = np.meshgrid(x,y)
    # shift the even rows: it will reshape the rectangular grid of points
    # into a triangular-like grid (see Fig 1). As in many other areas of
    # optimization, providing a good initial guess helps convergence
    # into the desired final grid.
    xx[1::2] = xx[1::2] + h0x/2.0   # shifts even rows by h0x/2
    # p: points of the grid
    p = np.zeros((np.size(xx),2))
    p[:,0] = np.reshape(xx,np.size(xx))
    p[:,1] = np.reshape(yy,np.size(yy))

    # remove points outside the shape boundary with distance > geps
    p = np.delete(p,np.where(fd(p) > geps),axis=0)

    # redistribute remaining points according to the weighting function
    # and then add the fixed points
    np.random.seed(random_seed)
    # probability to keep a point:
    r0 = 1.0/fh(p)**2
    p = np.concatenate((pfix,p[np.random.rand(len(p))<r0/max(r0),:]))

    pold = np.inf
    Num_of_Delaunay_triangulations = 0
    Num_of_Node_movements = 0  # dp = F*dt

    while (1):
        Num_of_Node_movements += 1
        if Iflag == 1 or Iflag == 3:  # Newton flag
            print 'Num_of_Node_movements = %3d' % (Num_of_Node_movements)
        if np.max(np.sqrt(np.sum((p - pold)**2,axis = 1))) > ttol:
            Num_of_Delaunay_triangulations += 1
            if Iflag == 1 or Iflag == 3:   # Delaunay flag
                print 'Num_of_Delaunay_triangulations = %3d' % \
                      (Num_of_Delaunay_triangulations)
            pold = p
            tri = Delaunay(p)  # instantiate a class
            t = tri.vertices
            pmid = (p[t[:,0]] + p[t[:,1]] + p[t[:,2]])/3.0
            # keep the triangles whose geometrical center is inside the shape
            t = t[np.where(fd(pmid) < -geps)]
            bars = np.concatenate((t[:,[0,1]],t[:,[0,2]], t[:,[1,2]]))
            # delete repeated bars
            bars = unique_rows(np.sort(bars))
            if Iflag == 4:
                min_q, min_angle_deg = triqual_flag(p,t)
                print 'Del iter: %3d, min q = %5.2f, min angle = %3.0f deg' \
                      % (Num_of_Delaunay_triangulations, min_q, min_angle_deg)
                if min_q > qmin:
                    break
            if Iflag == 2 or Iflag == 3:
                # graphical output of the current mesh
                ktrimesh(p,bars)

        # move mesh points based on bar lengths L and forces F

        barvec = p[bars[:,0],:] - p[bars[:,1],:]
        L = np.sqrt(np.sum(barvec**2,axis=1))
        hbars = 0.5*(fh(p[bars[:,0],:]) + fh(p[bars[:,1],:]))
        L0 = hbars*Fscale*np.sqrt(np.sum(L**2)/np.sum(hbars**2))
        F = np.maximum(L0-L,0)
        Fvec = np.column_stack((F,F))*(barvec/np.column_stack((L,L)))
        # Fvec: bar forces, (x,y) components

        # Calculate now the total force on a given point p due to its
        # interaction with all the bars joining point p to neighboring points

        Ftot = np.zeros((len(p),2))
        n = len(bars)
        for j in range(n):
            # sum of forces on the 1st point of a bar
            Ftot[bars[j,0],:] += Fvec[j,:]  # the : for the (x,y) components
            # but the same point may be on the other side of a bar, so we
            # sum of forces on the 2nd point of a bar
            Ftot[bars[j,1],:] -= Fvec[j,:]
            # the minus sign is because by our convention, Fvec is the force
            # on the first node of a bar. Hence, the force on the 2nd node
            # of a bar is -Fvec (Netwon's action-reaction law)

        # force = 0 at fixed points, so they do not move:
        Ftot[0: len(pfix),:] = 0

        # update the node positions
        p = p + deltat*Ftot

        # bring outside points back to the boundary
        d = fd(p); ix = d > 0   # find points outside (d > 0)
        dpx = np.column_stack((p[ix,0] + deps,p[ix,1]))
        dgradx = (fd(dpx) - d[ix])/deps
        dpy = np.column_stack((p[ix,0], p[ix,1] + deps))
        dgrady = (fd(dpy) - d[ix])/deps
        p[ix,:] = p[ix,:] - np.column_stack((dgradx*d[ix], dgrady*d[ix]))

        # termination criterium: all interior nodes move less than dptol:

        if max(np.sqrt(np.sum(deltat*Ftot[d<-geps,:]**2,axis=1))/h0) < dptol:
            break

    final_tri = Delaunay(p)  # another instantiation of the class
    t = final_tri.vertices
    pmid = (p[t[:,0]] + p[t[:,1]] + p[t[:,2]])/3.0
    # keep the triangles whose geometrical center is inside the shape
    t = t[np.where(fd(pmid) < -geps)]
    bars = np.concatenate((t[:,[0,1]],t[:,[0,2]], t[:,[1,2]]))
    # delete repeated bars
    bars = unique_rows(np.sort(bars))
    # orient all the triangles counterclockwise (ccw)
    t = ccw_tri(p,t)
    # graphical output of the current mesh
    ktrimesh(p,bars)
    triqual(p,t,fh)
    return p,t,bars

# ------------------------------------------------------------------
#               END OF DISTMESH
# ------------------------------------------------------------------

# Note from Jaime Kardontchik:

# In rare situations the Delaunay function 'forgets' to enumerate one
# or a few isolated triangles in the generated mesh.
# A missing triangle in an otherwise complete mesh will appear to the
# Euler solver as an additional boundary (usually internal to the mesh)
# In these rare cases the 'Visual Boundary Integrity Check' plot will
# show up twice: the 1st time with the missing triangle(s). The second
# time with the problem corrected (no missing triangles). 
# Sometimes it might be difficult to see the missing triangle in the 
# 1st plot (because it might be very tiny) and you will have to use 
# the zoom to find it in the plot.
# The following modules were developed to correct this problem:

def boundary_bars(t):
    """
        input: t, the triangles generated by a Delaunay triangulation
        returns: an array with the boundary bars. Each element of the
        array consists of two integers identifying the nodes of the bar
    """       
    # create the bars (edges) of every triangle
    bars = np.concatenate((t[:,[0,1]],t[:,[0,2]], t[:,[1,2]]))
    # sort all the bars
    data = np.sort(bars)
    # find the bars that are not repeated
    Delaunay_bars = dict()
    for row in data:
        row = tuple(row)
        if row in Delaunay_bars:
            Delaunay_bars[row] += 1
        else:
            Delaunay_bars[row] = 1
    # return the keys of Delaunay_bars whose value is 1 (non-repeated bars)
    bbars = []
    for key in Delaunay_bars:
        if Delaunay_bars[key] == 1:
            bbars.append(key)
    bbars = np.asarray(bbars)
    return bbars   

def Delaunay_correction(p,t,bbars):
    """
    Finds missing triangles and eliminates spurious boundary bars.
    Appends the missing triangles to the t-array and returns the
    correct boundary bars and t-array.

    Algorithm: Find the missing triangles
    If a triplet of bbars is made up of only 3 nodes (p points) they
    constitute a missing small triangle. Example, if
    bbars[59] = np.array([3, 8])
    bbars[83] = np.array([8, 17])
    bbars[4]  = np.array([3, 17])
    Then the three bbars form a small missing triangle:
    t[missing] = np.array([3,8,17])
    Once a missing triangle is found and added, the bars forming this
    triangle are not in the boundary anymore, so they are deleted from
    the original array of bbars.
    """

    # the following is a simple one-time operation, so vectorizing it
    # would only make it difficult to understand, so it is not worthy
    bb_len = len(bbars)
    bars_to_delete = []
    tri_to_add = []
    
    for i in range(bb_len):
        for j in range(i+1,bb_len):
            for k in range(j+1,bb_len):
                # create a list of the nodes included in bbars i,j,k
                lis = []
                lis.extend(list(bbars[i]))
                lis.extend(list(bbars[j]))
                lis.extend(list(bbars[k]))
                # eliminate duplicates in this list
                lis = list(set(lis))  # class set removes duplicates from seq
                # if len(lis) == 3 we found a missing small triangle
                # the indexes of the bbars to delete are i,j,k
                # the nodes of the missing triangle are specified in lis
                if len(lis) == 3:
                    bars_to_delete.extend([i,j,k])
                    tri_to_add.append(lis)
    # now we proceed to add triangles to the t-array with nodes specified
    # in tri_to_add

    triangles_to_add = np.asarray(tri_to_add)
    if np.size(triangles_to_add) == 0:
        Tflag = 0
        correct_tri = ccw_tri(p,t)
    else:
        Tflag = 1
        correct_tri = np.concatenate((t,triangles_to_add), axis=0)
        correct_tri = ccw_tri(p,correct_tri)        

    # And  delete the bbars whose indexes are specified in bars_to_delete
    mask = [0,0]*len(bbars)  # element i of bbars is a 2-number array
    for i in bars_to_delete:
        mask[2*i] = 1
        mask[2*i+1] = 1
    bbars = np.ma.masked_array(bbars,mask)
    boundary_bars = bbars.compressed()  # delete the masked elements
    # reshape from (2n,0) to (n,2)
    len_bb = len(boundary_bars)/2
    boundary_bars = np.reshape(boundary_bars,(len_bb,-1))
    
    return correct_tri,boundary_bars,Tflag

def find_boundary(p,t,fh):
    bbars = boundary_bars(t)
    ktrimesh(p,bbars,pflag=1)
    correct_tri, correct_boundary_bars,Tflag = Delaunay_correction(p,t,bbars)
    if Tflag == 1:
        ktrimesh(p,correct_boundary_bars,pflag=1)
        triqual(p,correct_tri,fh)
    return correct_tri, correct_boundary_bars

def mesh_repair(p,t,fh,indexes):
    """
    Another Delaunay bug is the possible formation of thin spurious
    slivers in the boundary with clear violation of the triangulation
    rules: spurious bars crossing bars belonging to real triangles to
    join far away nodes and creating a spurious triangle. The mesh
    can be repaired by entering the indexes of the spurious triangles
    as a list, for example:
       indexes = [1,11,88,90,131,132]
    and then running the 'mesh_repair' module. 'indexes' is used as
    an argument to the 'mesh_repair' function.
    The mesh_repair module returns the array of true triangles.
    """
    indexes = np.asarray(indexes)
    # eliminate the spurious triangles
    t = np.delete(t,indexes,axis=0)
    # recreate the bars
    bars = np.concatenate((t[:,[0,1]],t[:,[0,2]], t[:,[1,2]]))
    # delete repeated bars
    bars = unique_rows(np.sort(bars))
    # recreate the mesh
    ktrimesh(p,bars)
    # re-calculate the mesh quality
    triqual(p,t,fh)
    return t
