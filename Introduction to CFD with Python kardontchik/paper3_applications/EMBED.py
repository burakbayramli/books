"""
    EMBED

Note:EMBED uses a slightly modified version of DISTMESH with some additional
    modules (modification of DISTMESH and additional modules created by
    Jaime Kardontchik, PhD) 
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay

from DISTMESH import *

#---------------------------------------------------------------------
#       Note about embedding an elliptical shape in a triangular mesh
# --------------------------------------------------------------------
"""    
    Given an ellipse with center at (xc,yc) and semiaxes (a,b), a > b,
    It generates N equidistant points on this ellipse.
    It returns the distance (h) between successive points and the
    coordinates of these points (verts). Notice that this distance is the
    cartesian distance between points and not the actual distance on the
    elliptical curve, but for large N the difference is small.

    Note: analytical calculation of the perimeter of an ellipse is very
          complicated. See:
             Chandrupatla and Osler, 'The perimeter of an ellipse',
             Math. Scientist 35, 122-131(2010)
          In Uniform_Embed, I avoid this approach by using a simple
          integration to calculate the perimeter of the ellipse

          A useful approximate formula that gives accurate results appears
          in: http://www.mathsisfun.com/geometry/ellipse-perimeter.html
          I used this formula in Simple_Embed
"""
class Uniform_Embed:
    def __init__(self,xc,yc,a,b,N):
        self.xc, self.yc, self.a, self.b = xc, yc, a, b
        self.N = N

    def __call__(self):
        """
        Generates N points equally spaced on the ellipse (verts)
        Returns the distance (h) between successive points and the
        coordinates of the nodes (verts)
        """
        xc, yc, a, b = self.xc, self.yc, self.a, self.b
        N = self.N
        # ----------------------------------------------------------
        #           calculate the array s
        # ----------------------------------------------------------
        #s[i] = length of the ellipe curve from point i=0 to point i
        #s[-1] = perimeter 
        M = 200000
        points = np.zeros((M,2))
        t = np.linspace(0,2*pi,M)
        points[:,0] = xc + a*np.cos(t)
        points[:,1] = yc + b*np.sin(t)
        s = np.zeros(M-1)
        for i in range(1,len(s)):
            delta = np.sqrt((points[i,0] - points[i-1,0])**2 + \
                  (points[i,1] - points[i-1,1])**2)
            s[i] = s[i-1] + delta

        # --------------------------------------------------------
        #       find N equally-separated points on the ellipse 
        # --------------------------------------------------------
        
        verts = np.zeros((N,2))   # points on the ellipse
        verts[0,0] = xc + a; verts[0,1] = yc # 1s point on the ellipse
        # distance h between successive points on the ellipse curve
        h = s[-1]/N

        k = 1
        for i in range(len(s) - 10):
            if s[i] > k*h:
                verts[k] = points[i]
                k = k + 1
        return h, verts

class Simple_Embed:
    def __init__(self,xc,yc,a,b,N):
        self.xc, self.yc, self.a, self.b = xc, yc, a, b
        self.N = N

    def __call__(self):
        xc, yc, a, b = self.xc, self.yc, self.a, self.b
        N = self.N

        verts = np.zeros((N,2))
        t = np.linspace(0,2*pi*(N-1)/N,N)
        verts[:,0] = xc + a*np.cos(t)
        verts[:,1] = yc + b*np.sin(t)
        # calculation of approx distance between successive points
        # Calculate the perimeter of the ellipse
        d = ((a - b)/(a+b))**2
        c1 = 1.0/4; c2 = 1.0/64; c3 = 1.0/256; c4 = 25.0/16384; c5 = 49.0/65536
        Per = pi*(a+b)*(1.0 + c1*d + c2*d**2 + c3*d**3 + c4*d**4 + c5*d**5)
        h  = Per/N  # coarse average distance between points
        return h, verts


# ----------------------------------------------------
#               CLASS Embed_Dist
# ----------------------------------------------------
"""
    Embed_Dist is a distance function used as an input argument to
    distmesh_embed. It is called from Main as follows:
            fd_embed = Embed_Dist(ellipse_verts)
"""

class Embed_Dist:
    def __init__(self,ellipse_verts):
        self.ellipse_verts = ellipse_verts
    def __call__(self,p):
        ellipse_verts = self.ellipse_verts
        dist = np.zeros(len(p))
        d = np.zeros((len(p),len(ellipse_verts)))
        for i in range(len(ellipse_verts)):
            d[:,i] = np.sqrt((p[:,0] - ellipse_verts[i,0])**2 + (p[:,1] - ellipse_verts[i,1])**2)
        dist = np.min(d,axis=1)
        return dist

"""
   DISTMESH was developed by Dr Per-Olof Persson, Department of Mathematics,
   Massachusetts Institute of Technology

   The original version of DISTMESH was written in MATLAB.

   This Python version of DISTMESH was developed by Dr Jaime Kardontchik,
   Sunnyvale, CA

   distmesh_embed is a modification of DISTMESH by Jaime Kardontchik to allow
   the automatic generation of high-quality meshes for airfoils
  
"""

# --------------------------------------------------------------------
#                   distmesh_embed
# --------------------------------------------------------------------

def distmesh_embed(fd_embed,fd,fh,h0,xmin,ymin,xmax,ymax,pfix,ttol=0.1,dptol=0.001,Iflag=1,qmin=1.0):
    #Iflag = 0: do not print internal sim status and
    #           do not plot intermediate meshes
    #Iflag = 1:(default) print internal simulation status only
    #Iflag = 2: plot intermediate meshes only
    #Iflag = 3: print internal sim status and plot intermediate meshes
    #Iflag = 4: prints the Delaunay iteration, minimum q and angle(deg)
    
    # Constants
    geps_embed = 0.65*h0
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
    # remove points within a distance < geps_embed from the embedded region
    p = np.delete(p,np.where(fd_embed(p) < geps_embed),axis=0)

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
#               END OF distmesh_embed
# ------------------------------------------------------------------
class Ears:
    """
    Finds if there are 'pointed ears' in the internal boundary and
    eliminates them generating the correct tri and the correct int_bnodes
    and int_bbars arrays.
    It reorders the int_bnodes and int_bbars to simplify the generation
    of the body-fitted mesh
    """
    def __init__(self,N,int_bnodes,int_bbars,tri,p):
        self.N, self.int_bnodes, self.int_bbars = N,int_bnodes,int_bbars
        self.tri, self.p = tri, p

    def __call__(self):
        N, int_bnodes,int_bbars = self.N, self.int_bnodes, self.int_bbars
        tri, p = self.tri, self.p
        number_of_ears = len(int_bnodes) - N
        if number_of_ears != 0:
            # --------------------------------
            #   add the missing triangles
            # --------------------------------
            # find the ears
            ind = np.where(int_bnodes >= N)
            out_nodes = list(ind[0])
            add_tri = np.zeros((len(out_nodes),3),dtype=int)
            for i in range(number_of_ears):
                # add the triangles in ccw
                if int_bnodes[out_nodes[i]] == int_bnodes[0]:
                    add_tri[i] = [int_bnodes[-1],int_bnodes[1],int_bnodes[0]]
                elif int_bnodes[out_nodes[i]] == int_bnodes[-1]:
                    add_tri[i] = [int_bnodes[-2],int_bnodes[0],int_bnodes[-1]]
                else:
                    add_tri[i] = [int_bnodes[out_nodes[i]-1],int_bnodes[out_nodes[i]+1], \
                                           int_bnodes[out_nodes[i]]]
            tri = np.concatenate((tri,add_tri))

        # -------------------------------------------------------
        #   correct the int_bnodes (if needed) and reorder them
        #   to coincide with the order of the airfoil nodes
        #   (from trail to head along the upper surface and back
        #   to trail along the lower surface)
        # --------------------------------------------------------
        int_bnodes = np.zeros(N,dtype = int)
        int_bnodes = np.arange(0,N)

        # -------------------------------
        #  same with the int_bbars
        # -------------------------------
        int_bbars = np.zeros((N,2),dtype = int)
        for i in range(0,N-1):
            int_bbars[i] = np.array( [int_bnodes[i], int_bnodes[i+1] ] )
        int_bbars[-1] = np.array([ [int_bnodes[-1], int_bnodes[0]] ])

        # -----------------------------
        #   plot the correct mesh
        # -----------------------------
        plt.figure()
        x = p[:,0]; y = p[:,1]
        for t in tri:
            t_i = [t[0],t[1],t[2],t[0]]
            plt.plot(x[t_i],y[t_i],'k')
            plt.hold('on')
        plt.title('FINAL MESH')
        plt.axis('equal')
        plt.hold('off')
        plt.show()
        return int_bnodes,int_bbars,tri
